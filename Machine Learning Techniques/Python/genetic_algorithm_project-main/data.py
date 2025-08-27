# Available rooms and their capacities
rooms = {
    'Slater 003': 45,
    'Roman 216': 30,
    'Loft 206': 75,
    'Roman 201': 50,
    'Loft 310': 108,
    'Beach 201': 60,
    'Beach 301': 75,
    'Logos 325': 450,
    'Frank 119': 60,
}

# Facilitators
facilitators = [
    'Lock', 'Glen', 'Banks', 'Richards', 'Shaw', 'Singer', 'Uther', 'Tyler', 'Numen', 'Zeldin'
]

# Activities and their attributes
activities = {
    'SLA100A': {
        'expected_enrollment': 50,
        'preferred_facilitators': ['Glen', 'Lock', 'Banks', 'Zeldin'],
        'other_facilitators': ['Numen', 'Richards'],
    },
    'SLA100B': {
        'expected_enrollment': 50,
        'preferred_facilitators': ['Glen', 'Lock', 'Banks', 'Zeldin'],
        'other_facilitators': ['Numen', 'Richards'],
    },
    'SLA191A': {
        'expected_enrollment': 50,
        'preferred_facilitators': ['Glen', 'Lock', 'Banks', 'Zeldin'],
        'other_facilitators': ['Numen', 'Richards'],
    },
    'SLA191B': {
        'expected_enrollment': 50,
        'preferred_facilitators': ['Glen', 'Lock', 'Banks', 'Zeldin'],
        'other_facilitators': ['Numen', 'Richards'],
    },
    'SLA201': {
        'expected_enrollment': 50,
        'preferred_facilitators': ['Glen', 'Banks', 'Zeldin', 'Shaw'],
        'other_facilitators': ['Numen', 'Richards', 'Singer'],
    },
    'SLA291': {
        'expected_enrollment': 50,
        'preferred_facilitators': ['Lock', 'Banks', 'Zeldin', 'Singer'],
        'other_facilitators': ['Numen', 'Richards', 'Shaw', 'Tyler'],
    },
    'SLA303': {
        'expected_enrollment': 60,
        'preferred_facilitators': ['Glen', 'Zeldin', 'Banks'],
        'other_facilitators': ['Numen', 'Singer', 'Shaw'],
    },
    'SLA304': {
        'expected_enrollment': 25,
        'preferred_facilitators': ['Glen', 'Banks', 'Tyler'],
        'other_facilitators': ['Numen', 'Singer', 'Shaw', 'Richards', 'Uther', 'Zeldin'],
    },
    'SLA394': {
        'expected_enrollment': 20,
        'preferred_facilitators': ['Tyler', 'Singer'],
        'other_facilitators': ['Richards', 'Zeldin'],
    },
    'SLA449': {
        'expected_enrollment': 60,
        'preferred_facilitators': ['Tyler', 'Singer', 'Shaw'],
        'other_facilitators': ['Zeldin', 'Uther', 'Richards', 'Banks'],
    },
    'SLA451': {
        'expected_enrollment': 100,
        'preferred_facilitators': ['Tyler', 'Singer', 'Shaw'],
        'other_facilitators': ['Zeldin', 'Uther', 'Richards', 'Banks'],
    },
}

# Available time slots
time_slots = ['10 AM', '11 AM', '12 PM', '1 PM', '2 PM', '3 PM']