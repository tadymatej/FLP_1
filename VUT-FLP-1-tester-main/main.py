import os
import subprocess
import sys

from config import N_TESTS, TASK_2_CLASSIFICATIONS

from generate_tests import generate
from gini import parse_tree_from_file, parse_tree_from_string, create_dataset, classify_new_data


def test_if_present(filepath):
    if not os.path.isfile(filepath):
        raise FileNotFoundError(f"No file found at {filepath}")
    else:
        print(f"File found at {filepath}")

def check_output(out_script, expected, idx):
    if out_script == expected:
        print(f'[ OK ] - correctly classified all data')
    else:
        print(f'[ BAD ] - test case: {idx}')
        print('##### EXPECTED #####')
        print(expected)
        print('##### ACTUAL #####')
        print(out_script)
        raise Exception("Invalid classification")

def run_task_1(idx):
    try:
        command = ["./flp-fun", "-1", "./tests/trees/tree_" + str(idx), "./tests/new_data/new_data_" + str(idx)]
        result = subprocess.run(command, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        if result.returncode != 0:
            raise ('flp-fun exited with non-zero return code')
        with open (f'./tests/classifications/classification_{idx}', 'r') as expected:
            # WINLUL CHANGE LINE HERE
            # check_output("\n".join(result.stdout.decode().splitlines()), expected.read().strip(), idx)
            check_output(result.stdout.decode(), expected.read(), idx)
            

    except subprocess.CalledProcessError as e:
        print(f"Error executing shell script: {e.stderr.decode()}", file=sys.stderr)
        raise Exception
    except Exception as e:
        print(f"An error occurred: {e}", file=sys.stderr)
        raise Exception
    
def run_task_2(idx):
    try:
        command = ["./flp-fun", "-2", f'./tests/training_data/data_{str(idx)}']
        result = subprocess.run(command, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        if result.returncode != 0:
            raise ('flp-fun exited with non-zero return code')

        t_expected = parse_tree_from_file(f'./tests/trees/tree_{idx}')[0]
        t_parsed = parse_tree_from_string(result.stdout.decode())[0]
        
        count_incorrect = 0
        count_total = TASK_2_CLASSIFICATIONS

        test_data = create_dataset(n_samples=count_total)
        classes_from_expected = classify_new_data(t_expected, test_data)
        classes_from_parsed = classify_new_data(t_parsed, test_data)
        
        for i in range(count_total):
            if classes_from_expected[i] != classes_from_parsed[i]:
                count_incorrect +=1 
        print(f'incorrect: {count_incorrect} ({(count_incorrect/count_total)*100}%)')
        return (count_incorrect/count_total)*100

    except subprocess.CalledProcessError as e:
        print(f"Error executing shell script: {e.stderr.decode()}", file=sys.stderr)
        raise Exception
    except Exception as e:
        print(f"An error occurred: {e}", file=sys.stderr)
        raise Exception
    
def make_files():
    command = ['make']
    result = subprocess.run(command, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if result.returncode != 0:
        raise Exception("Cannot make files")

if __name__ == "__main__":
    generate(N_TESTS)

    try:
        test_if_present('Makefile')
        test_if_present('flp-fun.hs')
        make_files()
        test_if_present('flp-fun')
        incorrect = []
        for i in range(1, N_TESTS + 1):
            print(f'Test #{i}')
            print('Task 1:', end='')
            run_task_1(i)
            print('Task 2:', end='')
            incorrect.append(run_task_2(i))
            print('----------')
        print("Avg incorrect: {}".format(sum(incorrect) / len(incorrect)))
        
    except Exception as e:
        print(e)
