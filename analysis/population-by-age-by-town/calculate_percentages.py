import csv
from collections import OrderedDict
from operator import itemgetter as i
from functools import cmp_to_key


def build_key(d):
    town = d['Town']
    year = d['Year'].replace(' ', '')
    race = d['Race/Ethnicity']
    gender = d['Gender']
    age = d['Age Cohort']
    mt = d['Measure Type']
    variable = d['Variable']
    return '{}_{}_{}_{}_{}_{}_{}'.format(town, year, race, gender, age, mt, variable)

def build_town_total_key(town, year, variable):
    return '{}_{}_All_Total_Total_Number_{}'.format(town, year, variable)


def aggregate_moe_compute(moe1, moe2):
    return round(((moe1**2) + (moe2**2))**(1/2), 4)


def derived_ratio_moe(numerator, denominator, numerator_moe, denominator_moe):
    try:
        prop = numerator / denominator
    except ZeroDivisionError:
        return 0.0
    moe = (((prop**2 * denominator_moe**2) + numerator_moe**2)**(1/2)) / denominator
    return round(moe, 4)

def proportional_moe_compute(numerator, denominator, numerator_moe, denominator_moe):
    try:
        prop = numerator / denominator
    except ZeroDivisionError:
        return 0.0
    moe = ((numerator_moe**2) - ((prop**2) * (denominator_moe**2)))**(1/2) / denominator
    try:
        return round(moe, 4)
    except TypeError:
        # As per ACS General Handbook, if this calc fails because val under sq root is negative,
        # we should used the derived ratios formula instead
        return derived_ratio_moe(numerator, denominator, numerator_moe, denominator_moe)


population = []
keys = []
lookup = {}

with open('raw/population-by-age-by-town-without-non-hispanic.csv') as f:
    reader = csv.DictReader(f)
    for row in reader:
        if row['Measure Type'] == 'Number':
            row['Value'] = float(row['Value'])
            population.append(row)
            key = build_key(row)
            lookup[key] = row

for k,v in lookup.items():
    keys.append(k)

final = []

for row in population:
    if row['Variable'] == 'Population':
        key = build_key(row)
        row_moe_lookup_key = key.replace("Population", "Margins of Error")
        total_lookup_key = build_town_total_key(row['Town'], row['Year'], 'Population')
        total_moe_lookup_key = build_town_total_key(row['Town'], row['Year'], 'Margins of Error')
        row_moe = lookup[row_moe_lookup_key]
        total = lookup[total_lookup_key]
        total_moe = lookup[total_moe_lookup_key]
        new_percent = OrderedDict(**row)
        new_percent['Measure Type'] = 'Percent'
        new_percent['Value'] = round(row['Value'] / total['Value'] * 100, 3)
        new_percent_moe = OrderedDict(**row)
        new_percent_moe['Measure Type'] = 'Percent'
        new_percent_moe['Variable'] = 'Margins of Error'
        new_percent_moe['Value'] = proportional_moe_compute(
            row['Value'],
            total['Value'],
            row_moe['Value'],
            total_moe['Value'])
        final.append(row)
        final.append(row_moe)
        final.append(new_percent)
        final.append(new_percent_moe)

with open('raw/population-by-age-recalculated.csv', 'w') as f:
    writer = csv.DictWriter(f, fieldnames=['Town', 'FIPS', 'Year', 'Gender', 'Race/Ethnicity', 'Age Cohort', 'Measure Type', 'Variable', 'Value'])
    writer.writeheader()
    writer.writerows(final)