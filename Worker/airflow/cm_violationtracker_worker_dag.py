from datetime import datetime, timedelta
from textwrap import dedent

from airflow import DAG
# from airflow.models import Variable

# from airflow.operators.bash import BashOperator
# from airflow.operators.python_operator import PythonOperator
from airflow.operators.docker_operator import DockerOperator

docker_conn_id_str  = "docker_aws_erc"
docker_host_url_str = "tcp://localhost:2376" # "unix://var/run/docker.sock"

# #-----------------------------------------------------------------
# def get_variable_fn(**kwargs):
#     cm_mapbox_api_token_str = Variable.get("cm_mapbox_api_token_dev", default_var="undefined")
#     print("cm_mapbox_api_token: ", cm_mapbox_api_token_str)
#     return cm_mapbox_api_token_str

#-----------------------------------------------------------------
dag = DAG(
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
    tags=['cm'])

# DOCKER_OPERATOR
t1 = DockerOperator(
    task_id='R_worker',
    image='833394423843.dkr.ecr.us-east-1.amazonaws.com/violationtracker_worker:dev',
    container_name="cm_violationtracker_r_worker_{{ ts_nodash }}",

    # command="/bin/sleep 10",

    environment={
        # "AWS_DEFAULT_REGION": "us-east-1",
        "MB_API_TOKEN": "{{ var.value.get('cm_mapbox_api_token_dev', 'undefined') }}",
        "WR_API_TOKEN": "{{ var.value.get('cm_waterreporter_api_key_dev', 'undefined') }}"
    },
    
    api_version='auto',

    # Allows to remove the Docker container as soon as the task is finished
    auto_remove=True,

    # always try to pull the latest version of the container, in case the image
    # got updated by a build system or manually by a developer.
    force_pull=True,

    docker_url=docker_host_url_str,

    #--------------------------
    # AIRFLOW_CONNECTION
    # ID of the Airflow connection to use
    docker_conn_id=docker_conn_id_str,

    #--------------------------

    # keep the worker container network isolated from the host it runs on
    network_mode="bridge",
    
    # MEMORY_CONSTRAINT
    # comment out if you dont want to limit how much memory this container is allowed
    # to consume at worst.
    mem_limit="1gb",
    
    dag=dag)