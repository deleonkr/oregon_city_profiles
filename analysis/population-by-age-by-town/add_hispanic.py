import csv
from operator import itemgetter as i
from functools import cmp_to_key

def cmp(a, b):
    return (a > b) - (a < b)

def multikeysort(items, columns):
    comparers = [
        ((i(col[1:].strip()), -1) if col.startswith('-') else (i(col.strip()), 1))
        for col in columns
    ]
    def comparer(left, right):
        comparer_iter = (
            cmp(fn(left), fn(right)) * mult
            for fn, mult in comparers
        )
        return next((result for result in comparer_iter if result), 0)
    return sorted(items, key=cmp_to_key(comparer))


def build_key(d):
    town = d['Town']
    year = d['Year'].replace(' ', '')
    gender = d['Gender']
    age = d['Age Cohort']
    mt = d['Measure Type']
    return '{}_{}_{}_{}_{}'.format(town, year, gender, age, mt)

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

complete = []
all_data = {}
all_moe = {}
hispanic_moe = {}
hispanic = []

with open('raw/population-by-age-recalculated.csv') as f:
    reader = csv.DictReader(f)
    for row in reader:
        row['Value'] = float(row['Value'])
        if row['Measure Type'] == 'Number':
            key = build_key(row)
            if row['Race/Ethnicity'] == 'All' and row['Variable'] == 'Population':
                all_data[key] = row
            if row['Race/Ethnicity'] == 'All' and row['Variable'] == 'Margins of Error':
                all_moe[key] = row
            if row['Race/Ethnicity'] == 'Hispanic or Latino' and row['Variable'] == 'Population':
                hispanic.append(row)
            if row['Race/Ethnicity'] == 'Hispanic or Latino' and row['Variable'] == 'Margins of Error':
                hispanic_moe[key] = row
        complete.append(row)

non_hispanic = []

for row in hispanic:
    key = build_key(row)
    h_moe = hispanic_moe[key]
    all_d = all_data[key]
    all_d_moe = all_moe[key]
    new = {**row}
    new['Race/Ethnicity'] = 'Non-Hispanic'
    new['Value'] = all_d['Value'] - row['Value']
    non_hispanic.append(new)
    new_moe = {**new}
    new_moe['Variable'] = 'Margins of Error'
    moe = aggregate_moe_compute(h_moe['Value'], all_d_moe['Value'])
    new_moe['Value'] = moe
    non_hispanic.append(new_moe)
    new_prop = {**new}
    new_prop['Measure Type'] = 'Percent'
    try:
        new_prop['Value'] = row['Value'] / all_d['Value']
    except ZeroDivisionError:
        new_prop['Value'] = 0.0
    non_hispanic.append(new_prop)
    new_prop_moe = {**new_prop}
    try:
        prop_moe = proportional_moe_compute(row['Value'], all_d['Value'], h_moe['Value'], all_d_moe['Value'])
    except TypeError as e:
        print(key)
    new_prop_moe['Value'] = prop_moe
    new_prop_moe['Variable'] = 'Margins of Error'
    non_hispanic.append(new_prop_moe)


complete = complete + non_hispanic

sorted_complete = multikeysort(
    complete,
    ['Year', 'Town', 'Gender', 'Race/Ethnicity', 'Age Cohort', 'Measure Type', 'Variable'])

with open('data/population-by-age-by-town.csv', 'w') as f:
    writer = csv.DictWriter(
        f,
        ['Year', 'FIPS', 'Town', 'Gender', 'Race/Ethnicity', 'Age Cohort', 'Measure Type', 'Variable', 'Value'])
    writer.writeheader()
    writer.writerows(sorted_complete)

