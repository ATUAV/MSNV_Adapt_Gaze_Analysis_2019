'''
Scirpt for generating relevant and non-relevant bar AOIs. 
In each .aoi file generated for a msnv, all lines except the last correspond to 
1 bar or the legend, with the aoi name set to its id in the json files. 

The last line is a semi-colon seperated concatenation of all the above 
AOIs' coordinates (excl. the legend), with the addtion of number AOIs. Although
it does not actually define an AOI we want, this line is the only line 
used by another script in the next step.
'''
import pandas as pd
import json
import os.path


intervention_df = pd.read_csv("intervention.csv")
intervention_df['msnv'] = intervention_df['name'].str.split('_', 1).str[0]
intervention_grouped = intervention_df.groupby('msnv')

msnv_ids = [27, 60, 11, 30, 62, 72, 28, 74, 5, 20, 76, 66, 9, 3]
#msnv_ids = [76]
offsets = (604,80)

# (left, top, width, height)
# 30 is special, additional logic below 
margins = {27: (0,-2,0,4), 60: (0,0,0,0), 11: (0,-6,0,6), 30: (0,-1.5,0,3), 
           62: (0,0,0,0), 72: (0,-6,0,6), 28: (0,-1.5,0,3), 74: (0,-3,0,6), 
           5: (0,-4.5,0,9), 20: (0,-4.5,0,9), 76: (0,0,0,0), 66: (0,-4,6,8), 
           9: (0,-6,0,12), 3: (0,-4.5,0,9)}

for msnv, interventions in intervention_grouped:
    aois = []
    relevant_indices = []
    if int(msnv) not in msnv_ids:
        continue
    
    with open('data_updated/{}_updated.json'.format(msnv)) as f:
        file = json.load(f)
        marks = file['marks']['marks']
        #print(len(marks))
    
    for index, intervention in interventions.iterrows():
        arguments = json.loads(intervention['arguments'])
        id = arguments["id"]
        relevant_indices.append(id)
                           
    for bar in marks:
        if bar['id'] == 'legend':
            continue
        
        top_left_x = offsets[0] + bar['left'] - 1 + margins[int(msnv)][0]
        top_left_y = offsets[1] + bar['top'] - 1 + margins[int(msnv)][1]
        if int(msnv) == 30 and (bar['id'] not in relevant_indices) and (bar['id'] not in [14, 17, 21, 25, 29, 43, 49, 52]):
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
        
        aois.append({bar['id']: bar_coord})
            
    f_nr = open('non_relevant_aois_adjusted/{}_NR.aoi'.format(msnv), 'w')
    f_rel =  open('relevant_aois_adjusted/{}_Relevant.aoi'.format(msnv), 'w')
    rel_overall_list = non_rel_overall_list = 'overall_list'
    
    # add number AOIs if any
    if (int(msnv) in [20, 60, 62, 74, 76]) and os.path.exists('bars_number_aois/{}_R.aoi'.format(msnv)):
        rel_num = open('bars_number_aois/{}_R.aoi'.format(msnv)).read()
        rel_overall_list += ('\t' + rel_num)
    if (int(msnv) in [20, 60, 62, 74, 76]) and os.path.exists('bars_number_aois/{}_NR.aoi'.format(msnv)):
        nr_num = open('bars_number_aois/{}_NR.aoi'.format(msnv)).read()
        non_rel_overall_list += ('\t' + nr_num)
    
    for aoi in aois:
        aoi_name = next(iter(aoi))
        if aoi_name in relevant_indices:
            fout = f_rel
        else: 
            fout = f_nr
        
        aoi_coord = aoi[aoi_name]
        
        fout.write(str(aoi_name))
        for i in range(len(aoi_coord)):
            string = '\t' + str(aoi_coord[i][0]) + ',' + str(aoi_coord[i][1])
            fout.write(string)
            
            if aoi_name in relevant_indices:
                rel_overall_list += string
                if i == 3:
                    rel_overall_list += '\t;'
            else: 
                non_rel_overall_list += string
                if i == 3:
                    non_rel_overall_list += '\t;'
                
        fout.write('\n')
    
    f_rel.write(rel_overall_list)
    f_nr.write(non_rel_overall_list)
    f_rel.close()
    f_nr.close()
            