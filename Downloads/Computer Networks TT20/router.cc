#include <stdio.h>
#include <omnetpp.h>

#include "Router.h"

Define_Module(Router);

using namespace std;


// functions from the cModule interface 
void Router::initialize() {
  // Edited here: initialize infinite route costs/poison reverse/split horizon 
  INFINITE_ROUTE_COST = par("smallInfinity");
  SPLIT_HORIZON = par("useSplitHorizon");
  POISON_REVERSE = par("usePoisonReverse");

  // initialise a zero-route for this router to itself
  Route selfRoute;
  selfRoute.dest = getName();
  selfRoute.nextHop = getName();
  selfRoute.cost = 0.0;
  routes[getName()] = selfRoute;

  // setup the internal activity messages
  periodic_update_activity = 
    new cMessage("Periodic update to neighbours.");
  scheduleAt(simTime() + par("periodicUpdateInterval"), periodic_update_activity);

  message_sending_activity = 
    new cMessage("Periodic sending of message to other router.");
  string messageTo = par("messageTo");
  if (messageTo != "") {
    scheduleAt(simTime() + par("messageSendingInterval"), message_sending_activity);
  }

  check_alive_activity =
    new cMessage("Periodic checking of neighbours being alive.");
  scheduleAt(simTime() + par("checkAliveInterval"), check_alive_activity);
}

void Router::finish() { 
  printRoutingTable();

  // delete internal activity messages
  cancelEvent(periodic_update_activity);
  delete periodic_update_activity;
  cancelEvent(message_sending_activity);
  delete(message_sending_activity);
  cancelEvent(check_alive_activity);
  delete(check_alive_activity);
}

void Router::handleMessage(cMessage *msg) {
  if (simTime() > par("diesAt")) {
    // if router has died, do nothing.
    if(msg != periodic_update_activity && 
       msg != message_sending_activity &&
       msg != check_alive_activity) {
      // don't delete the above messages because we reuse them.
      // do delete messages coming from other routers.
      delete msg;
    }
    return;
  } else if (msg == check_alive_activity) {
    checkAliveActivity();
    scheduleAt(simTime() + par("checkAliveInterval"), 
               check_alive_activity);
  } else if (msg == periodic_update_activity) {
    sendRoutingUpdatesActivity();
    scheduleAt(simTime()+par("periodicUpdateInterval"), periodic_update_activity);

  } else if (msg == message_sending_activity) {
    sendMessageToOtherRouterActivity(par("messageTo"));
    scheduleAt(simTime() + par("messageSendingInterval"), message_sending_activity);

  } else {
    // non-internal messages
    if (msg->getKind() == UPDATE_PACKET) {
      updateRoutesActivity((UpdatePacket*) msg);
      lastSeen[msg->getSenderModule()->getName()] = msg->getSendingTime();
    } else if (msg->getKind() == MESSAGE_PACKET) {
      MessagePacket* p = (MessagePacket*) msg;
      EV << '@' << getName()
         << ": Message " << p->getId()
         << " arrived from " << p->getSource() 
         << ": [" << p->getData() << ']' << endl;
    }
    delete msg;
  }
}



// utility functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

void Router::printRoutingTable() {
  EV << '@' << getName() << ": Routing table:" << endl;
  for (map<string, Route>::iterator r = routes.begin(); r != routes.end(); ++r) {
    EV << "--" << r->second.nextHop << "--> " << r->second.dest
       << ": " <<  r->second.cost << endl;
  }
  EV << endl;
}

void Router::sendCost(string dest) {
  // Edited here: Adapt this function for "useSplitHorizon" and "usePoisonReverse"
  const auto route = routes.find(dest);
  if (route != routes.end()) {
    const auto r = route->second;
    const cGate *nextHopGate;
    if (r.nextHop == getName()) {
      nextHopGate = NULL;
    } else {
      nextHopGate = findGateForNeighbour(r.nextHop);
    }
    for (auto i = 0; i < gateSize("out"); i++) {
      const auto g = gate("out", i);
      if (!SPLIT_HORIZON || nextHopGate != g) { // If we're not using SH or the nexthop is not g, update packet as below
        UpdatePacket *p = new UpdatePacket("", UPDATE_PACKET);
        p->setSource(getName());
        p->setDestination(dest.c_str());
        if (POISON_REVERSE && nextHopGate == g) { //Poison Reverse to set infinite cost back
          p->setCost(INFINITE_ROUTE_COST);
        } else {
          p->setCost(r.cost);
        }
        send(p, g);
      }
    }
  } else {
    EV << "Error: Request to send cost to unknown router." << endl;
  }
}

cGate* Router::findGateForNeighbour(string n) {
  for (int i=0; i < gateSize("out"); i++) {
    cGate *g = gate("out", i);
    if (g->getNextGate()->getOwnerModule()->getName() == n) {
      return g;
    }
  }

  EV << "@" << getName() << ": NO_GATE. Cannot find out gate for router "
     << n << endl;
  return NULL;
}


// activity functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

void Router::sendRoutingUpdatesActivity() {
//Edited here: send whole routing table to neighbours
    for (auto route = routes.begin(); route != routes.end(); route++) {
    sendCost(route->second.dest); 
  }
}

void Router::checkAliveActivity() {
  for (map<string, simtime_t>::iterator ls = lastSeen.begin(); 
       ls != lastSeen.end(); ) {
    if(simTime() - ls->second > par("peerFailedTimeOut")) {
      EV << "@" << getName() << ": neighbour " << ls->first << " failed" << endl;
      // Neighbour has not been seen recently and is considered failed
      // Edited here: Update the routing table and inform neighbours
      for (auto route = routes.begin(); route != routes.end();) {
        auto &r = route->second;
        if (r.nextHop == ls->first) {
          r.cost = INFINITE_ROUTE_COST;
          sendCost(r.dest);
          route = routes.erase(route);
        } else {
          route++; // Increment the route if fine to do so
        }
      }
      // Erase from map so we don't report the same failure 
      ls = lastSeen.erase(ls);
    } else {
      ls++;
    }
  }
}

void Router::updateRoutesActivity(UpdatePacket* p) {
  EV << "@" << getName() <<": Received update from " << p->getSource()
     << ". route " << p->getSource() << " --> " << p->getDestination() 
     << ", cost " << p->getCost() << endl;
  // Edited: update routing table and advertise changes to neighbours 
  //       if received update packet makes this neccessary
  const string source = p->getSource();
  const string dest = p->getDestination();
  const double updatedCost = p->getCost() + 1;

  const auto route = routes.find(dest);

  if (route == routes.end()) { // New Host of Network
    if (updatedCost < INFINITE_ROUTE_COST) {
      routes[dest] = {dest, source, updatedCost};
      sendCost(dest);
    }
  } else {
    auto &r = route->second;
    if (updatedCost < r.cost) { // NewCost is better, update and send
      r.nextHop = source;
      r.cost = updatedCost;
      sendCost(dest);
    } else if (updatedCost > r.cost && source == r.nextHop) {
      r.cost = updatedCost;
      sendCost(dest);
      if (updatedCost >= INFINITE_ROUTE_COST) { //Remove route if exceeds limit
        routes.erase(route);
      }
    }
  }
}

void Router::sendMessageToOtherRouterActivity(string dest) {
  MessagePacket* p = new MessagePacket("", MESSAGE_PACKET);
  p->setSource(getName());
  p->setDestination(dest.c_str());
  char data[100];
  snprintf(data, 100, "This is a message from %s to %s.", getName(), dest.c_str());
  p->setData(data);

  // send off the message
  if (routes.find(dest) != routes.end()) {  
    send(p, findGateForNeighbour(routes.find(dest)->second.nextHop));
  }
}
