import copy
import random

from config import FEATURE_SIZE, EPS


class Leaf:
    def __init__(self, c):
        self.class_name = c
class Node:
   def __init__(self, idx, value, left, right):
      self.left:  Leaf | Node = left
      self.right: Leaf | Node = right
      self.index: int  = idx
      self.value: float = value

def create_class_list(n_classes=3):
    # Class1 .. ClassN
    return [f'Class{i}' for i in range(1, n_classes+1)]

def create_feature_vector(size, a=0.0, b=1000.0):
    # [3.14, 1,71, ...]
    return [round(random.uniform(a, b) + random.normalvariate(100, 50), 2) for _ in range(size)]

def create_dataset(n_samples=5, feature_size=FEATURE_SIZE, class_list=None):
    # [[3.14, 1.71, ...], [...]]
    if class_list is None:
        return [create_feature_vector(feature_size) for _ in range(n_samples)]
    # [[3.14, 1.71, ...], 'ClassX']
    return [[create_feature_vector(feature_size), random.choice(class_list)] for _ in range(n_samples)]

def get_midpoints(feature_values):
    mps = []
    feature_values = sorted(feature_values)
    for i in range(len(feature_values) - 1):
       mps.append(round(((feature_values[i] + feature_values[i+1]) / 2), 3)) 
    return mps

def get_midpoints_2d(dataset):
    # transpose dataset
    dT = [list(i) for i in zip(*[x[0] for x in dataset])]
    return [get_midpoints(x) for x in dT]

def calculate_gini(groups, classes):
    n_instances = float(sum([len(group) for group in groups]))
    gini = 0.0
    for group in groups:
        size = float(len(group))
        if size == 0:
            continue
        score = 0.0
        for class_val in classes:
            p = [row[-1] for row in group].count(class_val) / size
            score += p * p
        gini += (1.0 - score) * (size / n_instances)
    return gini

def get_classes(dataset):
    return [x[-1] for x in dataset]

def split_dataset(dataset, feature_idx, midpoint):
    smaller =[]
    greater = []
    for dato in dataset:
        if dato[0][feature_idx] <= midpoint:
            smaller.append(dato)
        else:
            greater.append(dato)

    return smaller, greater

def get_best_split(dataset):
    best_gini = 1
    best_midpoint = None
    best_feature_idx = None

    midpoints = get_midpoints_2d(dataset)
    ginis = copy.deepcopy(midpoints)

    for feature_idx, feature_midpoints in enumerate(midpoints):
        for midpoint_idx, midpoint in enumerate(feature_midpoints):
            smaller, greater = split_dataset(dataset, feature_idx, midpoint)
            gini = calculate_gini([smaller, greater], list(set(get_classes(dataset))))
            ginis[feature_idx][midpoint_idx] = gini
            if gini < best_gini:
                best_gini = gini
                best_midpoint = midpoint
                best_feature_idx = feature_idx

    return best_feature_idx, best_midpoint

def construct_tree(dataset):
    classes = list(set(get_classes(dataset)))
    # there is only one class left, create leaf and return
    if len(classes) == 1:
        return Leaf(classes[0])
    
    best_feature_idx, best_midpoint = get_best_split(dataset)
    ld, rd = split_dataset(dataset, best_feature_idx, best_midpoint)

    return Node(best_feature_idx, best_midpoint, construct_tree(ld), construct_tree(rd))

def print_tree(tree: Node | Leaf, level=0, file=None):
    if isinstance(tree, Leaf):
        print(f'{" " * level}Leaf: {tree.class_name}', file=file)
        return
    print(f'{" "* level}Node: {tree.index}, {tree.value}', file=file)
    print_tree(tree.left, level + 2, file)
    print_tree(tree.right, level + 2, file)

def classify(tree: Node | Leaf, features_vector):
    if isinstance(tree, Leaf):
        return tree.class_name
    
    if features_vector[tree.index] <= tree.value:
        return classify(tree.left, features_vector)
    
    return classify(tree.right, features_vector)

def classify_new_data(tree, new_data):
    classes = []
    for new_dato in new_data:
        classes.append(classify(tree, new_dato))
    return classes

def parse_tree(input_arr):
    try:
        if input_arr[0] == 'Leaf:':
            class_name = input_arr[1]
            input_arr = input_arr[2:]
            return Leaf(class_name), input_arr
        elif input_arr[0] == 'Node:':
            left, rest = parse_tree(input_arr[3:])
            right, rest = parse_tree(rest)
            node_idx = int(input_arr[1])
            node_val = float(input_arr[2])
            return Node(node_idx, node_val, left, right), rest
    except Exception as e:
        raise Exception('Unabel to parse tree')
    
def flatten(arr):
    return [item for row in arr for item in row]

def parse_tree_from_string(s):
    tokens = [x.strip(',') for x in flatten([x.strip().split(' ') for x in s.split('\n')])]
    try:
        return parse_tree(tokens)
    except Exception as e:
        print()
        print(s)
        raise Exception('Unable to parse tree')

def parse_tree_from_file(filename):
    with open(filename) as f:
        data = f.readlines()
        ls = [x.strip() for x in data]
        ls = flatten([x.split(' ') for x in ls])
        ls = [x.strip(',') for x in ls]
        try:
            return parse_tree(ls)
        except Exception as e:
            print()
            for i in data:
                print(i, end='')
            raise Exception('Unable to parse tree')
    
def compare_trees(t1, t2):
    if isinstance(t1, Leaf) and isinstance(t2, Leaf):
        if t1.class_name != t2.class_name:
            raise Exception
    elif isinstance(t1, Node) and isinstance(t2, Node):
        if t1.index != t2.index:
            raise Exception
        diff = abs(t1.value - t2.value)
        if diff > EPS:
            raise Exception
        try:
            compare_trees(t1.left, t2.left)
            compare_trees(t1.right, t2.right)
        except Exception as e:
            raise Exception
    else:
        raise Exception

if __name__ == '__main__':
    ds = create_dataset(feature_size=FEATURE_SIZE, n_samples=10, class_list=create_class_list(n_classes=10))

    t = construct_tree(ds)
    print_tree(t)
    
    new_data = create_dataset(feature_size=FEATURE_SIZE, n_samples=1)
