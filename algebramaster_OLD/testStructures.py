import sympy

from src.algebramaster_OLD.structures import *


(a, b) = sympy.symbols("a, b")


class SubDictTester:
    def testSubDictCanEqualDict(self):
        origDict = {a: 2, b: 5}
        subDict = SubDict(origDict)
        
        assert subDict == origDict
    
    def testSubDictUsesConditionsWhenTestingEquals(self):
        origDict = {a: 2, b: 5}
        subDictWithoutConditions = SubDict(origDict)
        subDictWithConditions = SubDict(origDict, {a: 2})
        
        assert subDictWithConditions != origDict
        assert origDict != subDictWithConditions
        assert subDictWithConditions != subDictWithoutConditions
        assert subDictWithoutConditions != subDictWithConditions
        assert subDictWithoutConditions == origDict
        assert origDict == subDictWithoutConditions

class SubDictListTester:
    def testSubDictListCanCreateFromDictList(self):
        origList = [{a: 2, b: 5}, {a: -2, b: -5}]
        subDictList = SubDictList.fromList(origList)

        assert type(subDictList) is SubDictList
        for subDict in subDictList:
            assert type(subDict) is SubDict

    def testSubDictListCanCreateFromArgsList(self):
        condition_a = {a: 2}
        condition_b = {b: -5}
        origList = [({a: 2, b: 5}, condition_a), ({a: -2, b: -5}, condition_b)]
        subDictList = SubDictList.fromList(origList)

        assert type(subDictList) is SubDictList
        foundCondition_a = False
        foundCondition_b = False
        for subDict in subDictList:
            assert type(subDict) is SubDict
            if subDict.conditions == condition_a:
                foundCondition_a = True
            elif subDict.conditions == condition_b:
                foundCondition_b = True
        assert foundCondition_a and foundCondition_b

    def testSubDictListCanCreateFromMixedList(self):
        condition_b = {b: -5}
        origList = [{a: 2, b: 5}, ({a: -2, b: -5}, condition_b)]
        subDictList = SubDictList.fromList(origList)

        assert type(subDictList) is SubDictList
        foundCondition_b = False
        for subDict in subDictList:
            assert type(subDict) is SubDict
            if subDict.conditions == condition_b:
                foundCondition_b = True
            else:
                assert subDict.conditions == dict()
        assert foundCondition_b

    def testSubDictListCanEqualListOfDicts(self):
        origList = [{a: 2, b: 5}, {a: -2, b: -5}]
        subDictList = SubDictList.fromList(origList)

        assert subDictList == origList
        assert origList == subDictList

    def testSubDictListEqualsLikeSets(self):
        origList = [{a: 2, b: 5}, {a: -2, b: -5}]
        subDictList = SubDictList.fromList(origList)

        origListReversed = [{a: -2, b: -5}, {a: 2, b: 5}]

        assert subDictList == origListReversed
        assert origListReversed == subDictList
