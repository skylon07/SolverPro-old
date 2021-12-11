class immutable:
    _immutables = dict()
    _immutableClasses = list()

    # ran when python "calls the decorator"
    def __new__(cls, anyCallable):
        isClass = type(anyCallable) is type
        if isClass:
            class ImmutableMixin(anyCallable):
                # we want "calls" to the class to run the memo function
                def __new__(mixinCls, *args, **kwargs):
                    return cls._getMemo(mixinCls, args, kwargs, create=mixinCls.__createNew)

                def __init__(self, *args, **kwargs):
                    # DONT call parent init after __new__; we want to have control of that
                    return

                # the creation function is separate to avoid recursion errors
                # (and also gives a constant reference for memoizing)
                @classmethod
                def __createNew(mixinCls, *args, **kwargs):
                    newObj = super(ImmutableMixin, mixinCls).__new__(mixinCls)
                    super(mixinCls, newObj).__init__(*args, **kwargs)
                    return newObj
            cls._immutableClasses.append(ImmutableMixin)
            return ImmutableMixin
        else:
            def memoWrapper(*args, **kwargs):
                return cls._getMemo(anyCallable, args, kwargs)
            return memoWrapper

    @classmethod
    def _getMemo(cls, someCallable, argsTuple, kwargsDict, create=None):
        if len(kwargsDict) > 0:
            # currently no way to distinguish if an argument becomes a keyword argument
            # def fn(arg, kwargs="val")...
            # fn(1, 2) <-- args=(1,2) kwargs={}
            raise ValueError("Immutables cannot be created/called with keyword arguments")
        
        # separate creation function is needed for class wrapping
        if create is None:
            create = someCallable
        memoKey = cls._makeCallId(someCallable, argsTuple, kwargsDict)
        if memoKey not in cls._immutables:
            cls._immutables[memoKey] = create(*argsTuple, **kwargsDict)
        return cls._immutables[memoKey]

    @classmethod
    def _makeCallId(cls, someCallable, argsTuple, kwargsDict):
        callHash = hash(someCallable)
        hashableTuple = tuple(cls._makeHashable(item) for item in argsTuple)
        argsHash = hash(hashableTuple)
        # kwargsHash ignored; kwargs not currently allowed
        return hash((callHash, argsHash))

    @classmethod
    def _makeHashable(cls, obj):
        # sorts unordered data types for hashing
        if isinstance(obj, set):
            # TODO: what if items aren't comparable? ex 2 < '3'
            hashed = sorted(cls._makeHashable(item) for item in obj)
            return "set:{}".format(hashed)
        if isinstance(obj, dict):
            # key is already hashable to be in dict
            hashed = sorted((key, cls._makeHashable(obj[key])) for key in obj)
            return "dict:{}".format(hashed)
        
        # treat lists as tuples
        if isinstance(obj, list):
            hashed = [cls._makeHashable(item) for item in obj]
            return "list:{}".format(hashed)
        
        # if object isn't hashable, this will error eventually (just add a case in this function)
        return obj