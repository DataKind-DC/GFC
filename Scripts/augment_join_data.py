from dateutil import parser
import os
import re

import pandas as pd

activity_inputs_df = pd.read_excel(os.path.abspath(os.path.join(
    os.path.dirname(__file__),
    '../Raw Data (dont edit)/Data Export of Activity Inputs September 10.xlsx'
)))

capacity_building_types = list(activity_inputs_df['Capacity Building Type'].unique())

print "Capacity building types are {0}".format(capacity_building_types)

additional_columns = [
    'num_' + re.sub('[^0-9a-zA-Z]+', '_', item).lower()
    for item
    in capacity_building_types
]

additional_columns.extend([
    'weighted_score_' + re.sub('[^0-9a-zA-Z]+', '_', item).lower()
    for item
    in capacity_building_types
])

additional_columns.insert(0, 'done_date_year')

print "Additional columns are {0}".format(additional_columns)

min_done_date_year = parser.parse(str(activity_inputs_df['Done Date'].min())).year
max_done_date_year = parser.parse(str(activity_inputs_df['Done Date'].max())).year

print "Years for done dates for grants start from {0} and end in {1}.".format(
    str(min_done_date_year),
    str(max_done_date_year)
)

years = [
    x
    for x
    in range(min_done_date_year, max_done_date_year + 1)
]

regression_df = pd.DataFrame(
    columns=additional_columns
)

regression_df['done_date_year'] = years
regression_df.set_index('done_date_year', inplace=True)

activity_inputs_df['Done Date'] = activity_inputs_df['Done Date'].apply(lambda datetime_: datetime_.year)

# Hardcode ranking activities as the dataset CSV is different from the one in the rankings CSV.
weights = {
    'Site Visit': 4,
    'Knowledge Exchange Participation': 1,
    'Leveraging': 3,
    'Phone Call': 6,
    'E-mail': 7,
    'Additional Touch': 5,
    'Legal Referral': 8,
    'Grantee-Led Convening Participation': 2
}

for year in years:
    for activity in capacity_building_types:
        value = activity_inputs_df[
            (
                activity_inputs_df['Capacity Building Type'] == activity
            ) &
            (
                activity_inputs_df['Done Date'] == year
            )
        ].shape[0]

        column_name = 'num_' + re.sub('[^0-9a-zA-Z]+', '_', activity).lower()
        regression_df.at[year, column_name] = value

        column_name_b = 'weighted_score_' + re.sub('[^0-9a-zA-Z]+', '_', activity).lower()

        if activity in weights:
            regression_df.at[year, column_name_b] = weights[activity] * value
        else:
            # Weight = 1.0 by default
            regression_df.at[year, column_name_b] = value

regression_df.to_csv('gfc_1.csv')
