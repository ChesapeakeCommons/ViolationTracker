from datetime import datetime, timedelta
from textwrap import dedent

from airflow import DAG
from airflow.models import Variable

from airflow.operators.bash import BashOperator
from airflow.operators.python_operator import PythonOperator
from airflow.operators.docker_operator import DockerOperator



docker_host_url_str = "tcp://localhost:2376" # "unix://var/run/docker.sock"

#-----------------------------------------------------------------
def get_variable_fn(**kwargs):
    cm_mapbox_api_token_str = Variable.get("cm_mapbox_api_token_dev", default_var="undefined")
    print("cm_mapbox_api_token: ", cm_mapbox_api_token_str)
    return cm_mapbox_api_token_str

#-----------------------------------------------------------------
with DAG(
    'cm_violation_tracker_worker',

    # These args will get passed on to each operator
    # You can override them on a per-task basis during operator initialization
    default_args={
        'depends_on_past': False,
        'email': ['airflow@example.com'],
        'email_on_failure': False,
        'email_on_retry': False,
        'retries': 1,
        'retry_delay': timedelta(minutes=5),
        # 'queue': 'bash_queue',
        # 'pool': 'backfill',
        # 'priority_weight': 10,
        # 'end_date': datetime(2016, 1, 1),
        # 'wait_for_downstream': False,
        # 'sla': timedelta(hours=2),
        # 'execution_timeout': timedelta(seconds=300),
        # 'on_failure_callback': some_function,
        # 'on_success_callback': some_other_function,
        # 'on_retry_callback': another_function,
        # 'sla_miss_callback': yet_another_function,
        # 'trigger_rule': 'all_success'
    },
    description='R worker packaged in a container',
    schedule_interval=timedelta(days=1),
    start_date=datetime(2022, 1, 1),
    catchup=False,
    tags=['cm'],
    
) as dag:

    # DOCKER_OPERATOR
    t2 = DockerOperator(
        task_id='R_worker',
        image='833394423843.dkr.ecr.us-east-1.amazonaws.com/violationtracker_worker:dev',
        # command="/bin/sleep 10",

        environment={
            "AWS_DEFAULT_REGION": "us-east-1",
            "MB_API_TOKEN": "{{ var.value.get('cm_mapbox_api_token_dev', 'undefined') }}",
            "WR_API_TOKEN": "{{ var.value.get('cm_waterreporter_api_key_dev', 'undefined') }}"
        },
        
        api_version='auto',

        # Allows to remove the Docker container as soon as the task is finished
        auto_remove=True,
        docker_url=docker_host_url_str,

        # keep the worker container network isolated from the host it runs on
        network_mode="bridge"
    )