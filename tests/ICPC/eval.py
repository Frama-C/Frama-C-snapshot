import sys
import math

testcase = [ [ [  50000, "moveto",  15 ],
               [  70000, "moveto", -55 ],
               [ 100000, "moveto",  30 ],
               [ 160000, "shutoff",  0 ] ],
             [ [  30000, "moveto",  25 ],
               [  50000, "moveby", -70 ],
               [ 160000, "moveto", -20 ],
               [ 180000, "shutoff",  0 ] ],
             [ [  60000, "moveto", -30 ],
               [ 100000, "moveto",  30 ],
               [ 140000, "moveto", -10 ],
               [ 180000, "shutoff",  0 ] ],
             [ [  60000, "moveto",  20 ],
               [ 100000, "moveto", -20 ],
               [ 110000, "moveto",  20 ] ] ]

def checkSpeed(time, angleDiff, timeDiff):
    if timeDiff != 0:
        speed = float(angleDiff) / (timeDiff / 1000.0)
        if math.fabs(angleDiff) > 6.0 and math.fabs(speed) < 1.0:
            print "%d: TOO SLOW / TARGET NOT REACHED!" % time
            return False
    return True

if __name__ == '__main__':
    if len (sys.argv) != 2:
        print >>sys.stderr, "usage: python " + sys.argv[0] + " <testcase_number>\n"
    
    tc = int(sys.argv[1])
    pos = 0
    header = None
    cmd = None
    lastVoltage = 0.0
    lastAngle = 0.0
    lastTime = 0.0
    startAngle = 0.0
    voltageStableCounter = 0
    targetReached = False#
    errorCount = 0
    index = {}
    for line in sys.stdin.readlines():
        line = line.strip("\n")
        if header == None:
            header = line.split()
            for i in range(0, len(header)):
                index[header[i]] = i
        else:
            data = line.split()
            time = int(data[index["time(ms)"]])
            voltage = float(data[index["engineVoltage"]])
            angle = float(data[index["legAngle"]])
            angleValid = int(data[index["legAngleValid"]]) == 1
            isActive = int(data[index["isActive"]]) == 1
            if pos < len(testcase[tc]) and time >= testcase[tc][pos][0]:
                if not targetReached and cmd != None:
                    if not checkSpeed(time, cmd[2]-startAngle, time-cmd[0]):
                        errorCount += 1
                cmd = testcase[tc][pos]
                if cmd[1] == "moveby":
                    cmd[1] = "moveto"
                    cmd[2] = testcase[tc][pos-1][2] + cmd[2]
                pos += 1
                targetReached = False
                startAngle = angle
            moves = math.fabs(angle - lastAngle) > 0.1
            voltageZero = math.fabs(voltage - lastVoltage) < 0.1
            if voltageZero:
                voltageStableCounter += 1
            else:
                voltageStableCounter = 0
            voltageDiff = math.fabs(voltage - lastVoltage)
            if cmd != None and math.fabs(voltageDiff) > 2.0:
                print "%d: VOLTAGE JUMP!" % time
                errorCount += 1
            if cmd != None and voltageStableCounter >= 5 and not moves and not targetReached:
                if math.fabs(angle - cmd[2]) < 0.5:
                    targetReached = True
                    if not checkSpeed(time, cmd[2] - startAngle, time - cmd[0]):
                        errorCount += 1
            lastAngle = angle
            lastVoltage = voltage
            lastTime = time
    if not targetReached and cmd != None:
        if not checkSpeed(time, time-cmd[0], cmd[2]-startAngle):
            errorCount += 1

    print "found %d error(s)" % (errorCount)
