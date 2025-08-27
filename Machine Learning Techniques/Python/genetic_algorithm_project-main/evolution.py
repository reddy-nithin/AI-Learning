import random
from data import rooms, facilitators, time_slots


# Selection
def softmax_selection(population, normalized_scores):
    selected_schedules = []

    while len(selected_schedules) < len(population):
        selected_index = random.choices(range(len(population)), weights=normalized_scores, k=1)[0]
        selected_schedule = population[selected_index]

        # Check if any room in the selected schedule exceeds capacity
        room_usage = {room: 0 for room in rooms}

        valid_schedule = True
        for activity in selected_schedule:
            room = activity['room']
            if room_usage[room] + activity['expected_enrollment'] > rooms[room]:
                valid_schedule = False
                break
            room_usage[room] += activity['expected_enrollment']

        if valid_schedule:
            selected_schedules.append(selected_schedule)

    return selected_schedules

# Crossover
def pmx_crossover(parent1, parent2):
    # Initialize child schedules with the same structure as parents
    child1 = [None] * len(parent1)
    child2 = [None] * len(parent2)

    # Choose two random crossover points
    point1 = random.randint(0, len(parent1) - 2)
    point2 = random.randint(point1 + 1, len(parent1) - 1)

    # Copy the selected portion from parent1 to child1, and from parent2 to child2
    child1[point1:point2 + 1] = parent1[point1:point2 + 1]
    child2[point1:point2 + 1] = parent2[point1:point2 + 1]

    # Fill in the remaining slots in child1 and child2
    for i in range(len(parent1)):
        if i < point1 or i > point2:
            # Find values from parent2 that are not in child1
            while parent2[i] in child1[point1:point2 + 1]:
                i = parent2.index(parent1[i])
            child1[i] = parent2[i]

            # Find values from parent1 that are not in child2
            while parent1[i] in child2[point1:point2 + 1]:
                i = parent1.index(parent2[i])
            child2[i] = parent1[i]

    return child1, child2

# Mutation
def mutate_schedule(schedule):
    mutated_schedule = schedule.copy()

    # Select a random activity to mutate
    activity_index = random.randint(0, len(mutated_schedule) - 1)
    mutated_activity = mutated_schedule[activity_index]

    # Mutate time and facilitator (room assignment remains unchanged)
    mutation_type = random.choice(["time", "facilitator"])

    if mutation_type == "time":
        # Change the time to a random valid time slot
        mutated_schedule[activity_index]['time'] = random.choice(time_slots)
    else:
        # Change the facilitator to a random valid facilitator
        mutated_schedule[activity_index]['facilitator'] = random.choice(facilitators)

    return mutated_schedule