import random
from scipy.special import softmax
import json

# Required imports for Main function
from data import rooms, facilitators, activities, time_slots
from fitness_func import fitness
from evolution import softmax_selection, pmx_crossover, mutate_schedule

from heatmap_plot import generate_heatplot




# Define the parameters and data
population_size = 500
mutation_rate = 0.01  # Initial mutation rate


# Define the number of generations for testing (smaller value)
test_generations = 500  # A smaller number of generations for testing


# Define the chromosome structure and initialize the population
def generate_fixed_room_schedule():
    schedule = []
    available_rooms = list(rooms.keys())

    for activity_name, activity_attrs in activities.items():
        expected_enrollment = activity_attrs['expected_enrollment']
        preferred_facilitators = activity_attrs['preferred_facilitators']
        other_facilitators = activity_attrs['other_facilitators']
        facilitator = random.choice(facilitators)
        time = random.choice(time_slots)

        # Find a suitable room based on expected enrollment
        suitable_rooms = [room for room, capacity in rooms.items() if capacity >= expected_enrollment]
        oversized_rooms = [room for room, capacity in rooms.items() if capacity >= 3 * expected_enrollment]

        if suitable_rooms:
            room = random.choice(suitable_rooms)
        elif oversized_rooms:
            room = random.choice(oversized_rooms)
        else:
            continue

        schedule.append({
            'activity_name': activity_name,
            'room': room,
            'time': time,
            'facilitator': facilitator,
            'room_size': rooms[room],
            'expected_enrollment': expected_enrollment,
            'preferred_facilitators': preferred_facilitators,
            'other_facilitators': other_facilitators,
        })

    return schedule


population = [generate_fixed_room_schedule() for _ in range(population_size)]

# Initialize variables for tracking average fitness improvement
min_generations = 100
improvement_threshold = 0.01  # 1% improvement threshold
fitness_improvement_count = 0


children = []
mutated_schedules = []

# Main loop for evolution
generations = 0
best_fitness = -float('inf')

while generations < test_generations:

    # Calculate fitness for each schedule
    fitness_scores = [fitness(schedule) for schedule in population]

    # Softmax normalization (calculated at the beginning of each generation)
    normalized_scores = softmax(fitness_scores)

    # Selection
    selected_schedules = softmax_selection(population, normalized_scores)  # Implement selection logic here

    # Crossover
    parent1 = population[0]
    parent2 = population[1]
    child1, child2 = pmx_crossover(parent1, parent2)

    # Mutation
    original_schedule = population[0]
    mutated_schedule = mutate_schedule(original_schedule)

    # Check for termination criteria
    average_fitness = sum(fitness_scores) / population_size
    if generations >= min_generations:
        improvement_ratio = (average_fitness - best_fitness) / best_fitness
        if improvement_ratio <= improvement_threshold:
            fitness_improvement_count += 1
        else:
            fitness_improvement_count = 0

        if fitness_improvement_count >= min_generations:
            break

    # Replace the old population with the new population
    population = selected_schedules + children + mutated_schedules

    generations += 1

# Find and print the best schedule
best_schedule = population[fitness_scores.index(max(fitness_scores))]
best_fitness = max(fitness_scores)


# Extract room names and time slots
room_names = list(rooms.keys())
time_slots = time_slots

# Generate and display the heatmap
data_for_heatmap = [[0] * len(time_slots) for _ in range(len(room_names))]

for entry in best_schedule:
    room_index = room_names.index(entry['room'])
    time_index = time_slots.index(entry['time'])
    entry['expected_enrollment'] = int(entry['expected_enrollment'])
    data_for_heatmap[room_index][time_index] = entry['expected_enrollment']


# Heatmap Plotting
generate_heatplot(data_for_heatmap, time_slots, room_names)

print("Best schedule after", generations, "generations:")
print(json.dumps(best_schedule, indent=2))
print("Best fitness:", best_fitness)
