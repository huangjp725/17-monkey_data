# coding=utf-8

import pandas as pd

file_name = "../源代码和数据/朝阳医院2016年销售数据.xlsx"

xls_file = pd.ExcelFile(file_name)

print type(xls_file)

print xls_file

table = xls_file.parse('Sheet1')

print type(table)
