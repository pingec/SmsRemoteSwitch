#SmsRemoteSwitch

This was my first electronics/microcontroller project. The main purpose was to get acquainted with the AVR architecture. This is why it is written in AVR assembly from scratch without any third party libraries.

However, it does solve an actual problem. It is actively used for monitoring temperatures at a remote cottage and turning on the heating system before arrival.

This is actually not the final production code, it seems I have lost that one, but it is close enough, it is the version that ran on the prototype from the photo below and is kept here as a memento from the past.

![working prototype](https://raw.github.com/pingec/SmsRemoteSwitch/master/photo/prototype.jpg)

The code is for an ATmega168A chip which is used to interface a GSM terminal over RS232, toggle relay switches and acquire temperature readings from 1wire DS18B20 sensors. 
It can receive SMS commands to toggle relay switches and it supports SMS queries to report back temperatures through connected 1wire sensors.

#Supported SMS commands
- Help - list available commands
- GetTemp - returns temperature readings from all sensors
- SwStatus - returns the states of the connected relay switches
- SetSwitch - turns specified switches ON and returns the same as SwStatus 

  ```SetSwitch0123``` will turn switches 0,1,2,3 ON
- ClearSwitch - opposite of SetSwitch (turns switches off)


