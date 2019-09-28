#!/usr/bin/python

from __future__ import print_function
import sys
from collections import namedtuple

MachineEvent = namedtuple('MachineEvent', ['time', 'machineId', 'type',
                                           'platform', 'cpu', 'mem'])

class MachineInfo:
    def __init__(self, cpu, mem):
        try:
            self.cpu = float(cpu)
        except:
            self.cpu = 0
        try:
            self.mem = float(mem)
        except:
            self.mem = 0

def extractMachinesInfo(f):
    machines = {}
    for line in f:
        token = line.split(',')
        event = MachineEvent._make(token)

        if event.type == '0': # added 
            machine = MachineInfo(event.cpu, event.mem)
            machines[event.machineId] = machine 
            print(event.time, event.type, event.machineId, machine.cpu,
                  machine.mem)
        elif event.type == '1': # removed
            machine = machines[event.machineId]
            print(event.time, event.type, event.machineId, -machine.cpu,
                  -machine.mem)
            del machines[event.machineId]
        elif event.type == '2': # updated
            machine = machines[event.machineId]
            newInfo = MachineInfo(event.cpu, event.mem)
            print(event.time, event.type, event.machineId,
                  machine.cpu - newInfo.cpu, machine.mem - newInfo.mem)
            machines[event.machineId] = newInfo 
        else: # invalid type 
            raise

def main(argv):
    for inputFile in argv:
        f = open(inputFile,"r")
        extractMachinesInfo(f)

if __name__ == "__main__":
    main(sys.argv[1:])

