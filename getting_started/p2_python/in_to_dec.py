import math
import re
import time

stop_program = "stop"

while True:
    measurement = input("enter measurement")
    if measurement == stop_program:
        break
    
    else:
        num = re.findall(r'\d+', measurement)
        num_list = list(map(int, num))
        list_count = int(len(num_list))

        if list_count == 4:
            base_feet_frac = num_list[0]
            base_inches_frac = num_list[1]
            numerator = num_list[2]
            denominator = num_list[-1]
            inch_dec = numerator / denominator
            inches = base_inches_frac + inch_dec
            feet_dec_frac = inches / 12
            #rounds to 2 decimal places
            feet_frac = round(base_feet_frac + feet_dec_frac,2)     
            print(feet_frac)

        elif list_count == 2:
            base_feet = num_list[0]
            base_inches = num_list[1]
            feet_dec = base_inches / 12
            feet = base_feet + feet_dec
            print(feet)

        else:
            print("invalid, use feet-inch format (ie 10\'8\" or 10\'8 1/2\")")