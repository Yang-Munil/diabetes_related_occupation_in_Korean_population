import os
from datetime import datetime
from dateutil.relativedelta import relativedelta

to_ym_date = datetime.today()
run_base_ymd = datetime.strftime(to_ym_date, "%Y%m%d")
ym_list = [datetime.strftime(datetime.today() + relativedelta(months=-i), "%Y%m") for i in range(0,24)]
ym_list=["202004"]

for ym in ym_list:
    os.chdir("/Users/open/PycharmProjects/env_scrapy/real_estate_data")
    os.system("""source /Users/open/PycharmProjects/env_scrapy/venv/bin/activate;
                 scrapy crawl --logfile=./log/result_{}.log -a ym={} collect_real_estate """.format(run_base_ymd, ym))
