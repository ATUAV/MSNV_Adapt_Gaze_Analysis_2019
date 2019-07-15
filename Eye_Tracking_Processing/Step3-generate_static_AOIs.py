"""
Generates all lists of bars that would be highlighted for all interventions, 
including the AOIs for the numbers outside the bars. 

Every 4 (x,y) coordinates in each line is a bar for an intervention. Each 
AOI's name is the rule name from the database. 1 file per msnv. 
"""
import pandas as pd
import json


rule_intervention_df = pd.read_csv("rule_intervention_payload.csv")
rule_payload = rule_intervention_df.groupby('rule_name')
rules_ignored = ["5_ref_109_rule", "5_ref_110_rule", "5_ref_106_rule", "30_ref_102_rule", "62_ref_102_rule"]

intervention_df = pd.read_csv("intervention.csv")

msnv_ids = [27, 60, 11, 30, 62, 72, 28, 74, 5, 20, 76, 66, 9, 3]
#msnv_ids = [76]
offsets = (604,80)
# (left, top, width, height)
# 30 is special, additional logic below 
margins = {27: (0,-2,0,4), 60: (0,0,0,0), 11: (0,-6,0,6), 30: (0,-1.5,0,3), 
           62: (0,0,0,0), 72: (0,-6,0,6), 28: (0,-1.5,0,3), 74: (0,-3,0,6), 
           5: (0,-4.5,0,9), 20: (0,-4.5,0,9), 76: (0,0,0,0), 66: (0,-4,6,8), 
           9: (0,-6,0,12), 3: (0,-4.5,0,9)}
msnv_aois = {}
for msnv_id in msnv_ids:
    msnv_aois[msnv_id] = []


for rule, interventions in rule_payload:
    # get all AIOs for each intervention rule and combine them

    if (rule in rules_ignored):
        continue
    msnv = rule.split("_")[0]
    if (int(msnv) not in msnv_ids):
        continue
    print(rule)
    rule_coord = []
    
    with open('data_updated/{}_updated.json'.format(msnv)) as f:
        file = json.load(f)
        marks = file['marks']['marks']
    
    for index, intervention in interventions.iterrows():
        # print(intervention["intervention_name"])
        arguments = intervention_df[intervention_df.name == intervention['intervention_name']].iloc[0]['arguments']
        arguments = json.loads(arguments)
        
        id = arguments["id"]
        bar = next((x for x in marks if x['id'] == id), None)
        if (bar == None): 
            print('cannot find bar with id {} in msnv {}'.format(id, msnv))
        print(id, bar['id'])
        
        top_left_x = offsets[0] + bar['left'] - 1 + margins[int(msnv)][0]
        top_left_y = offsets[1] + bar['top'] - 1 + margins[int(msnv)][1]
        if int(msnv) == 30 and (bar['id'] not in [14, 17, 21, 25, 29, 43, 49, 52, 15, 9, 1, 5]):
            top_left_x -= 6

        top_right_x = top_left_x + bar['width'] + 2 + margins[int(msnv)][2]
        top_right_y = top_left_y
        if int(msnv) == 30:
            top_right_x += 6

        bottom_left_x = top_left_x
        bottom_left_y = top_left_y + bar['height'] + 2 + margins[int(msnv)][3]

        bottom_right_x = top_right_x
        bottom_right_y = bottom_left_y
        
        bar_coord = [(top_left_x, top_left_y), (top_right_x, top_right_y), 
                     (bottom_right_x, bottom_right_y), (bottom_left_x, bottom_left_y)]
        
        rule_coord.extend(bar_coord)
    
    msnv_aois[int(msnv)].append({rule: rule_coord})
 
    
for msnv, aois in msnv_aois.items():
    with open('static_aois_adjusted/{}_intervention.aoi'.format(msnv), 'w') as fout:
        for aoi in aois:
            aoi_name = next(iter(aoi))
            aoi_coord = aoi[aoi_name]
            
            fout.write(aoi_name)
            for vertex in aoi_coord:
                fout.write('\t' + str(vertex[0]) + ',' + str(vertex[1]))
            fout.write('\n')
            
        if int(msnv) in [20, 60, 62, 74, 76]:
            num = open('intervention_number_aois/{}.aoi'.format(msnv)).read()
            print(num)
            fout.write(num)
    
            
            
            
            
            
