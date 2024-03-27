import numpy as np
import pandas as pd
import math 
import sys

class TreeNode:
    def __init__(self, attribute_index=None, threshold=None, left=None, right=None, value=None):
        self.attribute_index = attribute_index
        self.threshold = threshold
        self.left = left
        self.right = right
        self.value = value

def gini_score(labels):
    unique_classes, counts = np.unique(labels, return_counts=True)
    probabilities = counts / len(labels)
    gini = 1 - np.sum(probabilities ** 2)
    return gini

def split_data(data, attribute_index, threshold):
    left_indices = data[:, attribute_index] <= threshold
    right_indices = ~left_indices
    left_data = data[left_indices]
    right_data = data[right_indices]
    return left_data, right_data

def find_best_split(data):
    best_gini = float('inf')
    best_attribute_index = None
    best_threshold = None
    n_features = data.shape[1] - 1
    for attribute_index in range(n_features):
        # TODO: V haskellu nebudu vůbec potřebovat funkci unique, ani vyfiltrovávat pouze hodnoty daného atributu.
        #       Stačí mi nejprve sortnout podle aI, poté tvořit dvě pole (odpředu a odzadu pro výpočet midpoints == sorted_values[1:], sorted_values[:-1])
        #       Tyto pole tvořit tak, že pokud přijde stejná hodnota attributu jako předchozí hodnota = skip (pouze pokračuj v rekurzi)
        #       Jinak vlož tuto hodnotu do polí pro výpočet midpoints (skip platí pro obě tvořené pole)
        #       Z těchto dvou polí lze potom provést výpočet midpoints jako na řádku 40 zde
        sorted_values = np.sort(np.unique(data[:, attribute_index]))
        # vse krome prvni hodnoty + vse krome posledni hodnoty (jako vektorovy soucet) / 2 => vektor
        midpoints = (sorted_values[1:] + sorted_values[:-1]) / 2.0
        print(sorted_values[1:] + sorted_values[:-1])
        for threshold in midpoints:
            left_labels = data[data[:, attribute_index] <= threshold][:, -1] #Vezme labely, které jsou menší rovny tresholdu daného atributu (pouze labels)
            right_labels = data[data[:, attribute_index] > threshold][:, -1]
            gini_left = gini_score(left_labels)
            gini_right = gini_score(right_labels)
            gini = (len(left_labels) * gini_left + len(right_labels) * gini_right) / len(data)
            if gini < best_gini:
                best_gini = gini
                best_attribute_index = attribute_index
                best_threshold = threshold
    return best_gini, best_attribute_index, best_threshold

def build_tree(data, depth=0, max_depth=None):
    labels = data[:, -1]
    unique_labels = np.unique(labels)
    if len(unique_labels) == 1:
        return TreeNode(value=unique_labels[0])
    if max_depth is not None and depth >= max_depth:
        return TreeNode(value=unique_labels[np.argmax(np.bincount(labels))])
    best_gini, attribute_index, threshold = find_best_split(data)
    if attribute_index is None or threshold is None:
        return TreeNode(value=unique_labels[np.argmax(np.bincount(labels))])
    left_data, right_data = split_data(data, attribute_index, threshold)
    left_subtree = build_tree(left_data, depth + 1, max_depth)
    right_subtree = build_tree(right_data, depth + 1, max_depth)
    return TreeNode(attribute_index=attribute_index, threshold=threshold, left=left_subtree, right=right_subtree)

def print_tree(node, indent=''):
    if node.value is not None:
        print(indent + 'Leaf:', node.value)
    else:
        print(indent + 'Node:', f'{node.attribute_index},', f'{node.threshold}')
        print_tree(node.left, indent + '  ')
        print_tree(node.right, indent + '  ')

def classify(row, node):
    if node.value is not None:
        return node.value
    if row[node.attribute_index] <= node.threshold:
        return classify(row, node.left)
    else:
        return classify(row, node.right)


fileName = sys.argv[1]
fileName = "./tests/training_data/data_100"
training_data = pd.read_csv(fileName)

decision_tree = build_tree(training_data.values)

print_tree(decision_tree)
