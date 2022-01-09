__requiredLockedFor = dict()
__requiredCallsPerClass = dict()
__requiredCallsLeftPerObj = dict()

def _getClassId(cls):
    return cls.__qualname__

def _getClassIdForMethod(clsFn):
    callPath = clsFn.__qualname__
    clsId = ".".join(callPath.split(".")[:-1])
    return clsId

def _getObjId(obj):
    return id(obj)

def _getClassIdFromObj(obj):
    return _getClassId(obj.__class__)

def _getClassCallsSet(clsId):
    mustCallSet = __requiredCallsPerClass.get(clsId)
    if mustCallSet is None:
        mustCallSet = __requiredCallsPerClass[clsId] = set()
    return mustCallSet

def _getObjCallsSet(obj):
    objId = _getObjId(obj)
    mustCallSet = __requiredCallsLeftPerObj.get(objId)
    if mustCallSet is None:
        classId = _getClassIdFromObj(obj)
        baseSet = _getClassCallsSet(classId)
        mustCallSet = __requiredCallsLeftPerObj[objId] = set(baseSet)
    return mustCallSet

def _markIsRequired(clsFn):
    clsId = _getClassIdForMethod(clsFn)
    if __requiredLockedFor.get(clsId):
        raise RuntimeError("Tried to mark a method as required after creating instances of the same class")
    
    mustCallSet = _getClassCallsSet(clsId)
    mustCallSet.add(clsFn)

def _markRequiredCalled(obj, clsFn):
    clsId = _getClassIdFromObj(obj)
    __requiredLockedFor[clsId] = True

    mustCallSet = _getObjCallsSet(obj)
    mustCallSet.remove(clsFn)

class Requireable:
    pass # TODO

# decorator that marks a function as "required" for ensured calls
def requiredCall(clsFn):
    # TODO: is there a way to ensure self is defined/ignore if it isn't?
    _markIsRequired(clsFn)
    def trackedFn(self, *args, **kwargs):
        _markRequiredCalled(self, clsFn)
        return clsFn(self, *args, **kwargs)
    return trackedFn

# decorator that checks all required functions have been called
def ensuredCall(clsFn):
    def ensuredFn(self, *args, **kwargs):
        mustCallSet = _getObjCallsSet(self)
        if len(mustCallSet) > 0:
            mustCallNames = ", ".join(fn.__name__ for fn in mustCallSet)
            raise RuntimeError("Required functions were not called: {}".format(mustCallNames))
        return clsFn(self, *args, **kwargs)
    return ensuredFn

if __name__ == "__main__":
    class A:
        @requiredCall
        def __init__(self):
            pass

        @requiredCall
        def runBefore(self):
            pass

        @ensuredCall
        def fn(self):
            pass

    class B(A):
        def __init__(self):
            pass
    b = B()
    b.fn()
