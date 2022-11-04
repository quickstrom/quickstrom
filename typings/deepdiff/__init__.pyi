from typing import Dict, Iterable, Tuple, Type, TypedDict

DiffPath = str

class ValueChangedDiff(TypedDict):
    old_value: object
    new_value: object

class TypeChangesDiff(TypedDict):
    old_type: Type
    new_type: Type
    old_value: object
    new_value: object

class DiffDict(TypedDict):
    values_changed: Dict[DiffPath, ValueChangedDiff]
    type_changes: Dict[DiffPath, TypeChangesDiff]
    iterable_item_added: Dict[DiffPath, object]
    iterable_item_removed: Dict[DiffPath, object]
    dictionary_item_added: Dict[DiffPath, object]
    dictionary_item_removed: Dict[DiffPath, object]

DeepDiff = DiffDict
