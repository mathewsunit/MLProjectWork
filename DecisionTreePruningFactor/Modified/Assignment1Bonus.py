#!/usr/bin/env python
# This is  my Decision Tree project

import math
import sys
import re
import random

def calcTestPer(tree,line):  # Test iteratively values in test set and measure accuracy by comparing with class of test set
    zero = 0
    one = 0
    for i, v in enumerate(line):
        length = len(v)
        if tree.testTree(line[0],v) == v[length-1]:
            zero = zero + 1
        else:
            one = one + 1
    return zero/(zero+one)

def split(line,k):  # split a test set into 2 set based on k column where one array has k =1 other is k=0
    """Return a list containing the Fibonacci series up to n."""
    length = len(line[0])-1
    tesline = [line[0][0:k]+line[0][k+1:length+1]]
    tes2line = [line[0][0:k]+line[0][k+1:length+1]]
    for i, v in enumerate(line[1:]):
            if v[k] == '0':
                tesline.append(v[0:k]+v[k+1:length+1])
            else:
                if v[k] == '1':
                    tes2line.append(v[0:k]+v[k+1:length+1])
    returnval = [tesline,tes2line]
    return(returnval)

def calcEntropy(line):  # calculate the entropy of a test set
    """Return a list containing the Fibonacci series up to n."""
    zero = 0
    one = 0
    length = len(line[0])
    for i, v in enumerate(line):
        if v[length-1] == '0':
             zero = zero + 1
        else:
            if v[length-1] == '1':
                 one = one + 1
    if(zero == 0 or one == 0):
        return 0
    else:
        ent = (-(zero/(zero+one)*math.log2(zero/(zero+one))))+(-(one/(zero+one))*math.log2(one/(zero+one)))
        return(ent)


class Node:
    def __init__(self, val, ent, zero, one):
        self.l = None
        self.r = None
        self.v = val
        self.e = ent
        self.z = zero
        self.o = one
        self.i = 0

class Tree:
    def __init__(self):
        self.root = None
    def getRoot(self):
        return self.root
    def add(self, val, ent, zero, one):
        if(self.root == None):
            self.root = Node(val, ent, zero, one)
        else:
            self._add(val, ent, zero, one, self.root)
    def _add(self, val, ent, zero, one, node):
            if(node.l != None):
                if(node.r != None):
                    self._add(val, ent, zero, one, node.l)
                else:
                    node.r = Node(val, ent, zero, one)
            else:
                node.l = Node(val,ent, zero, one)
    def printTree(self):
        if(self.root != None):
            self._printTree(self.root,1)
    def _printTree(self, node,numdash):
        if(node != None):
            if node.l.v == '0' or node.l.v =='1':
                for i in range(0,numdash):
                    print('|',end=" ")
                print(str(node.v)+" ID :"+str(node.i)+" = 0 : "+node.l.v, end="\n")
            else:
                for i in range(0,numdash):
                    print('|',end=" ")
                print(str(node.v)+" ID :"+str(node.i)+" = 0 :", end="\n")
                self._printTree(node.l,numdash+1)
            if node.r.v == '0' or node.r.v =='1':
                for i in range(0,numdash):
                    print('|',end=" ")
                print(str(node.v)+" ID :"+str(node.i)+" = 1 : "+node.r.v, end="\n")
            else:
                for i in range(0,numdash):
                    print('|',end=" ")
                print(str(node.v)+" ID :"+str(node.i)+" = 1 :", end="\n")
                self._printTree(node.r,numdash+1)
    def numberTree(self):
        if(self.root != None):
            self.root.i = 0
            self._numberTree(self.root,0)
    def _numberTree(self,node,num):
        if(node != None):
            node.i = num+1
            num = self._numberTree(node.l,num+1)
            return self._numberTree(node.r,num)
        else:
            return num
    def removeFromTree(self,i):
        if(self.root != None):
            self._removeFromTree(self.root,i)
    def _removeFromTree(self,node,i):
        if(node != None):
            if(node.l != None):
                if(node.l.i == i):
                    if(node.l.z > node.l.o):
                        node.l.z = node.l.z + node.l.o
                        node.l.o = 0
                        node.l.v = '0'
                        node.l.e = 100
                        node.l.r = None
                        node.l.l = None
                    else:
                        node.l.o = node.l.z + node.l.o
                        node.l.z = 0
                        node.l.v = '1'
                        node.l.e = 100
                        node.l.r = None
                        node.l.l = None
                else:
                    if(node.r != None):
                        if(node.r.i == i):
                            if(node.r.z > node.r.o):
                                node.r.z = node.r.z + node.r.o
                                node.r.o = 0
                                node.r.v = '0'
                                node.r.e = 100
                                node.r.r = None
                                node.r.l = None
                            else:
                                node.r.o = node.r.z + node.r.o
                                node.r.z = 0
                                node.r.v = '1'
                                node.r.e = 100
                                node.r.r = None
                                node.r.l = None
                        else:
                            self._removeFromTree(node.l,i)
                            self._removeFromTree(node.r,i)
    def getEntropy(self):
        if(self.root != None):
            return self._getEntropy(self.root)
    def _getEntropy(self,node):
        if(node != None):
            array = []
            nodeEnt = [node.i,node.e]
            array.append(nodeEnt)
            nodeLEnt = self._getEntropy(node.l)
            for i,v in enumerate(nodeLEnt):
                array.append(v)
            nodeREnt = self._getEntropy(node.r)
            for i,v in enumerate(nodeREnt):
                array.append(v)
            return array
        else:
            return []
    def getSize(self):
        if(self.root != None):
            return self._getSize(self.root)
    def _getSize(self,node):
        if(node != None):
            nodeSize = node.i
            nodeLSize = self._getSize(node.l)
            nodeRSize = self._getSize(node.r)
            maxSize = max(nodeSize,nodeLSize,nodeRSize)
            if nodeSize == maxSize:
                return nodeSize
            else:
                if nodeLSize == maxSize:
                    return nodeLSize
                else:
                    return nodeRSize
        else:
            return 0
    def getLeafNode(self):
        if(self.root != None):
            return self._getLeafNode(self.root)
    def _getLeafNode(self,node):
        if(node != None):
            if(node.v == '0') or (node.v == '1'):
                return 1
            else:
                return self._getLeafNode(node.l) + self._getLeafNode(node.r)
        else:
            return 0
    def getLeafDepth(self):
        if(self.root != None):
            return self._getLeafDepth(self.root,0)
    def _getLeafDepth(self,node,depth):
        if(node != None):
            if(node.v == '0') or (node.v == '1'):
                return depth+1
            else:
                return self._getLeafDepth(node.l,depth+1) + self._getLeafDepth(node.r,depth+1)
        else:
            return 0
    def testTree(self,ref,val):
        if(self.root != None):
            return self._testTree(self.root,ref,val)
    def _testTree(self,node,ref,val):
        length = len(ref)
        dictionary = dict(zip(ref, list(range(0,length-1))))
        if(node.v == '0' or node.v == '1'):
            return node.v
        if(val[dictionary[node.v]]=='0'):
            #print(node.v)
            return self._testTree(node.l,ref,val)
        else:
            return self._testTree(node.r,ref,val)

def prune(tree, pruneFactor): #prune tree by randomly choosing a a node from the three nodes that have the least entropy
    pruneInt = pruneFactor*tree.getSize()
    print("Number of nodes to be pruned = ",end=' ')
    print(pruneInt)
    blah = tree.getEntropy()
    for num in range(0,int(pruneInt)):
        blah.sort(key=lambda x: x[1])
        off = random.choice(blah[:3])
        tree.removeFromTree(off[0])
        print("Nodes to be pruned = ",end=' ')
        print(off[0])
        blah.remove(off)


def mergeTree(tree1,tree2,val,ent,zero, one): #merge 2 trees into one
    new = Tree()
    if tree1.root.v == tree2.root.v and (tree1.root.v=='0' or tree1.root.v=='1'):
        #print('collapsing')
        new.add(tree1.root.v,tree1.root.e,tree1.root.z,tree1.root.o)
    else:
        new.add(val,ent,zero, one)
        new.root.l = tree1.root
        new.root.r = tree2.root
    return new

def buildTree(val): #build a tree based on a training set
    entropyarray = []
    length = len(val[0])
    #print("length")
    #print(length)
    size = len(val)
    #print("size")
    #print(size)
    if length == 1:
        new = Tree()
        count0 = val.count(['0'])
        count1 = val.count(['1'])
        if count0>count1:
            new.add('0',100,count0,0)
        else:
            new.add('1',100,0,count1)
        return new
    if size == 1:
        new = Tree()
        if bool(random.getrandbits(1)):
            new.add('0',100,0,0)
        else:
            new.add('1',100,0,0)
        return new
    if calcEntropy(val) == 0:
        new = Tree()
        #print("val")
        #print(val)
        if(val[1][length-1] == '0'):
            new.add(val[1][length-1],100,size,0)
        else:
            if(val[1][length-1] == '1'):
                new.add(val[1][length-1],100,0,size)
        return new
    for num in range(0,length-1):
        mucho=split(val,num)
        mucho0len = len(mucho[0])-1
        mucho1len = len(mucho[1])-1
        weightedavg0 = mucho0len/(mucho0len+mucho1len)
        weightedavg1 = mucho1len/(mucho0len+mucho1len)
        entropyarray.append(calcEntropy(val)-(weightedavg0*calcEntropy(mucho[0]))+(weightedavg1*calcEntropy(mucho[1])))
    maxvaluetag = val[0][entropyarray.index(max(entropyarray))]
    maxvalueindex = entropyarray.index(max(entropyarray))
    mucho=split(val,maxvalueindex)
    tree1 = buildTree(mucho[0])
    tree2 = buildTree(mucho[1])
    return mergeTree(tree1,tree2,maxvaluetag,max(entropyarray),tree1.root.z+tree2.root.z,tree1.root.o+tree2.root.o)

def buildTreeRandom(val): #build a tree based on a training set
    entropyarray = []
    length = len(val[0])
    #print("length")
    #print(length)
    size = len(val)
    #print("size")
    #print(size)
    if length == 1:
        new = Tree()
        count0 = val.count(['0'])
        count1 = val.count(['1'])
        if count0>count1:
            new.add('0',100,count0,0)
        else:
            new.add('1',100,0,count1)
        return new
    if size == 1:
        new = Tree()
        if bool(random.getrandbits(1)):
            new.add('0',100,0,0)
        else:
            new.add('1',100,0,0)
        return new
    if calcEntropy(val) == 0:
        new = Tree()
        #print("val")
        #print(val)
        if(val[1][length-1] == '0'):
            new.add(val[1][length-1],100,size,0)
        else:
            if(val[1][length-1] == '1'):
                new.add(val[1][length-1],100,0,size)
        return new
    for num in range(0,length-1):
        mucho=split(val,num)
        mucho0len = len(mucho[0])-1
        mucho1len = len(mucho[1])-1
        weightedavg0 = mucho0len/(mucho0len+mucho1len)
        weightedavg1 = mucho1len/(mucho0len+mucho1len)
        entropyarray.append(calcEntropy(val)-(weightedavg0*calcEntropy(mucho[0]))+(weightedavg1*calcEntropy(mucho[1])))
    randChoice = random.choice(entropyarray)
    maxvaluetag = val[0][entropyarray.index(randChoice)]
    maxvalueindex = entropyarray.index(randChoice)
    mucho=split(val,maxvalueindex)
    tree1 = buildTree(mucho[0])
    tree2 = buildTree(mucho[1])
    return mergeTree(tree1,tree2,maxvaluetag,randChoice,tree1.root.z+tree2.root.z,tree1.root.o+tree2.root.o)


with open(sys.argv[1]) as textFile:
    train = [line.split() for line in textFile if len(line)>1]

with open(sys.argv[2]) as textFile:
    test = [line.split() for line in textFile if len(line)>1]

#for i,v in enumerate(train):
#    print(i,v)

print("\n\n")

tree = buildTree(train)
tree.numberTree()

print("--------------------------------")
print("ID3 Decision Tree")
print("--------------------------------")

tree.printTree()

print("\n\n")

print("--------------------------------")
print("ID3 Accuracy")
print("--------------------------------")
print("Number of training instances = ",end =" ")
print(len(train)-1)
print("Number of training attributes = ",end =" ")
print(len(train[0])-1)
print("Total number of nodes in the tree = ",end =" ")
print(tree.getSize())
print("Number of leaf nodes in the tree = ",end =" ")
print(tree.getLeafNode())
print("Accuracy of the model on the training dataset = ",end =" ")
print(calcTestPer(tree,train))
print("Number of testing instances = ",end =" ")
print(len(train)-1)
print("Number of testing attributes = ",end =" ")
print(len(train[0])-1)
print("Accuracy of the model on the testing dataset = ",end =" ")
print(calcTestPer(tree,test))
print("Average depth = ",end =" ")
print(tree.getLeafDepth()/tree.getLeafNode())

print("\n\n")

randomTree = buildTreeRandom(train)
randomTree.numberTree()

print("--------------------------------")
print("Non ID3 Decision Tree")
print("--------------------------------")

randomTree.printTree()

print("\n\n")

print("--------------------------------")
print("Non ID3 Accuracy")
print("--------------------------------")
print("Number of training instances = ",end =" ")
print(len(train)-1)
print("Number of training attributes = ",end =" ")
print(len(train[0])-1)
print("Total number of nodes in the tree = ",end =" ")
print(randomTree.getSize())
print("Number of leaf nodes in the tree = ",end =" ")
print(randomTree.getLeafNode())
print("Accuracy of the model on the training dataset = ",end =" ")
print(calcTestPer(randomTree,train))
print("Number of testing instances = ",end =" ")
print(len(train)-1)
print("Number of testing attributes = ",end =" ")
print(len(train[0])-1)
print("Accuracy of the model on the testing dataset = ",end =" ")
print(calcTestPer(randomTree,test))
print("Average depth = ",end =" ")
print(randomTree.getLeafDepth()/randomTree.getLeafNode())

print("\n\n")
