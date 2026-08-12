#!/usr/bin/env python
# coding: utf-8

# # **Project 3 - Reinforcement Learning**
# 
# 
# 
# 
# 

# ## 🌞 Mission: Find a secret Location in Trondheim where SkyNets servers are located

# 📊 **High-level goal:**
# 
# Train an agent (the “learner”) to interact with given environment and learn from it, so it can maximize its cumulative rewards over time.
# 
# 
# 
# 
# *   **Environment:**
#      *   The environment **is already created** and set up for you with the help of
# "gymnasium" library.
#      *   The agent is interacting with a simulated environment, which has states, accepts actions, and returns rewards. The environment resets at the beginning of each episode, and the agent takes actions within it, receiving feedback in the form of rewards.
# *   **Q-table:**
#      *   This table stores information about the "quality" of actions in different states.
#      *   The goal of the agent is to update the Q-table over time to estimate the value of taking different actions in various states.
#      *   You will initialize and learn Q-table in this project.
# 

# ## 📮 Imports:

# In[ ]:


# import libraries
import numpy as np
import matplotlib.pyplot as plt
import imageio.v3 as iio
import random
import os

# note: make sure to install necessary libraries like imageio, gymnasium etc.
# !pip install gymnasium


# In[ ]:


# import gymnasium stuff
import gymnasium as gym
from gymnasium import error, spaces, utils
from gymnasium.utils import seeding
from gymnasium.spaces import Discrete, MultiDiscrete


# ### Utility functions are provided for you:
# 
# 
# *   one_hot_decode
# *   one_hot_encode
# *   make_2d
# *   make_3d
# *   read_png_file
# *   is_valid_idx
# *   flatten_observation
# *   is_valid_idx
# *   flatten_observation
# *   make_2d_observation
# *   swap_values

# In[ ]:


def one_hot_decode(label_3D):
  """
  Converts back from one-hot encoded format (label_3D)
  to original class labels for each pixel (2D)
  """
  return np.argmax(label_3D, axis=-1)

def one_hot_encode(label_2D, label_values):
    """
    Converts a segmentation image label array (label_2D) to one-hot format
    by replacing each pixel value with a vector of length num_classes
    # Arguments
        label_2D: The 2D array segmentation image label
        label_values: an RGB array of classes (num_classes, 3)

    # Returns
        A 3D array with the same width and height as the input,
        but with a depth size of num_classes (for each pixel)

        Each class gets its own encoding for every pixel in the image.
        Encodings are then stacked along new axis which is number of classes
    """
    semantic_map = []
    for colour in label_values:
        equality = np.equal(label_2D, colour)
        class_map = np.all(equality, axis=-1)
        semantic_map.append(class_map)
    semantic_map = np.stack(semantic_map, axis=-1)

    return semantic_map

def make_2d(map_3d=None, palette_array=None):
    """
    Converts RGB images to images with number of class as value at [i,j]

    map_3d: 3D RGB image (H, W, n_ch = 3)
    palette_array: an RGB array of classes (num_classes, 3)

    Result: 2D structure where each "pixel" in the image
    has a class number instead of RGB color
    """
    # here we make 3D representation, 3rd dimension has one-hot encoded classes
    replaced_image_onehot = one_hot_encode(
        map_3d.astype(np.uint8), palette_array) # size (H, W, num_classes)
    # we create 2D representation, where every pixel has the value of its class
    return one_hot_decode(replaced_image_onehot) # size (H, W) - 2D

def make_3d(map_2d=None, palette_array=None):
    """
    Convert a 2d img to 3d
    Convert a 2D image where each pixel represents a class label
    into a 3D image where each class label is represented
    by its corresponding RGB color from the palette_array.
    Creates colorized 3D image from the original 2D class map

    map_2d: array of class labels, size (H, W)
    palette_array: an RGB array of classes (num_classes, 3)

    Result: RGB image (H, W, 3)
    """
    return palette_array[map_2d.astype(np.uint8)]


def read_png_file(image_path, printDebug = True):
    """
    read a png file and returns it as a 3D numpy array
    """
    original_image_matrix = iio.imread(image_path)

    if original_image_matrix.ndim > 2 and original_image_matrix.shape[-1] > 3:
        if printDebug:
            print(f'image has more than 3 channels, only first 3 channels are used there are {original_image_matrix.shape} channels')
        original_image_matrix = original_image_matrix[:, :, :3]
    return original_image_matrix

def is_valid_idx(image_2D, selected_idx):
    """
    Checks if a given index is within the valid range of a 2D array

    if selected_idx is valid in image_2D
    selected_idx = [i,j]
    image_2D = [h,w]
      """
    if selected_idx[0] < 0 or selected_idx[1] < 0:
        return False
    if selected_idx[0] < image_2D.shape[0]:
        if selected_idx[1] < image_2D.shape[1]:
            return True
    return False

def flatten_observation(observation_arr):
    """
    Converts/flattens a multi-dimensional array into a 1D array
    """
    return observation_arr.flatten


def make_2d_observation(observation_flattened, observation_arr_shape):
    """
    Converts flattened array into shape observation_arr_shape
    """
    return observation_flattened.reshape(observation_arr_shape.shape)

def swap_values(arr, value1, value2):
    """
    Swaps occurencies of two specific values (value1 and value2)
    within a 2D NumPy array (arr).

    arr: A 2D NumPy array where we want to swap the values
    value1: the first value that we want to swap with value2
    value2: the second value that we want to swap with value1

    Returns: 2D array with swapped values
    """

    # Ensure the values are integers
    value1, value2 = int(value1), int(value2)

    # Step 1: Find the first position of value1 and change it to value2
    pos_value1 = np.argwhere(arr == value1) # returns 2D array
    if pos_value1.size > 0:
        arr[pos_value1[0][0], pos_value1[0][1]] = value2

    # Step 2: Get all positions of value2
    pos_value2 = np.argwhere(arr == value2) # returns 2D array

    # Step 3: Randomly select a position of value2 that is not the original value1's position
    if len(pos_value2) > 1:
        # filter out the position of former value1 (pos_value1) which now also contains value2
        pos_value2 = pos_value2[np.all(pos_value2 != pos_value1[0], axis=1)]
        if pos_value2.size > 0:
            random_pos_value2 = pos_value2[np.random.choice(len(pos_value2))]
            arr[random_pos_value2[0], random_pos_value2[1]] = value1

    return arr


# ### Environment class with necessary functions - provided for you:
# 
# 
# *   constructor __init__
# *   step
# *   reset
# *   get_1d_state
# *   render
# *   close
# 

# In[ ]:


class TrondheimEnv(gym.Env):
    metadata = {'render.modes': ['human']}

    def __init__(self, conf):
        """
        Every environment should be derived from gym.Env and at least contain
        variables observation_space and action_space specifying the type of possible
        observations and actions using spaces.Box or spaces.Discrete.

        Example:
        >>> EnvTest = TrondheimEnv()
        >>> EnvTest.observation_space=spaces.Box(low=-1, high=1, shape=(3,4))
        >>> EnvTest.action_space=spaces.Discrete(2)
        """

        self.manual_mode = True  # Flag to control printouts

        # action definition:
        number_action = 4
        self.action_space = Discrete(number_action)

        self.env_image_path = conf['env_image_path']
        self.number_removable_locations = conf['number_removable_locations']

        self.pallete = np.array([[0, 0, 255],    # Blue:0:Water
                            [255, 255, 0],  # Yellow:1:Start
                            [0, 255, 0],    # Green:2:Land
                            [255, 0, 0],    # Red:3:BusLane
                            [128, 0, 128],  # Purple:4:BusStop
                            [0,   0,   0],  # Black:5:Secret Location
                            [128, 128, 0]], # Olive:6:Final Location
                           dtype=np.uint8)

        self.neighbours = np.array([[-1,0],[0,-1],[1,0],[0,1]]) # up, left, down, right
        self.not_allowed_area = [0] # water
        self.normal_land_class = 2 # land
        self.neutral_area = [self.normal_land_class,3] # land and BusLane
        self.start_class = 1 # start
        self.busstop_class = 4 # bus stop
        self.secret_location_class = 5 # secret location
        self.final_location_class = 6 # final location (2)

        # does agent die when enter not_allowed_area
        self.dead_allowed = conf['dead_allowed']
        # does shops closed after the first purchase
        self.remove_after_location_found = conf['remove_after_location_found']
        # should we start at different place each time
        self.start_random = conf['start_random']

        # read RGB image into a 3D numpy array
        img_3d = read_png_file(self.env_image_path, printDebug = False)

        # convert RGB image into 2D semantic class representation
        img_2d = make_2d(map_3d=img_3d,
                         palette_array=self.pallete)

        # the current state of the environment
        self.observation = img_2d # initial observation - 2D semantic class
        # copy of the environment's initial state before any actions have been taken
        self.initial_observation = img_2d.copy()

        max_observation_value = np.max(img_2d)
        # assert isinstance(max_observation_value, int)
        # can be used, not needed for this implementation
        # observation space is a gym object (MultiDiscrete), like numpy array. It can be sampled
        observation_space = np.full_like(img_2d,
                                         fill_value=max_observation_value)
        self.observation_space = MultiDiscrete(observation_space)

        # rewards
        self.reward = 0

        # done
        self.done = False

        # terminated
        self.terminated = False




    def step(self, action):
        """
        This method is the primary interface between environment and agent.

        Parameters:
            action: int
                    the index of the respective action (if action space is discrete)

        Returns:
            output: (array, float, bool, info)
                    information provided by the environment about its current state:
                    (observation, reward, done)

                    self.observation: The updated state of the environment (2D)
                    reward: Reward agent received for the action in this step (Int)
                    terminated: indicate if the episode has ended (Bool)
                    info: Additional information (not used currently)

        """
        obs = self.observation # 2D array of semantic class image representation
        terminated = False
        truncated = False
        info = None

        # just start and see where it goes
        start_idx = np.argwhere(obs == self.start_class)[0]
        # action is: 0:up, 1:left, 2:down, 3:right
        next_idx = start_idx + self.neighbours[action]
        reward = self.reward
        if self.manual_mode:
          print('Next step towards: ')

        if not is_valid_idx(image_2D=obs, selected_idx=next_idx):
            next_idx = start_idx
            if self.manual_mode:
                print('Hey there, you cannot escape outside of the environment')

        # now decide what happens next

        if obs[tuple(next_idx)] == self.normal_land_class:
            if self.manual_mode:
                print('Green Land class area')

        # check if it is a non allowed area
        if obs[tuple(next_idx)] in self.not_allowed_area:
            if self.manual_mode:
                print('The move is not allowed')
            if self.dead_allowed:
                # penalize the agent if allowed to die. Terminate
                reward = -1000
                terminated = True
            else:
                # if NOT allowed to die, reset the next position to current one - prevent the move
                next_idx = start_idx


        # if the player is in bus stup
        elif obs[tuple(next_idx)] == self.busstop_class:
            if self.manual_mode:
                print('A bus stop')

            # get the bus stop that is not the next step
            bus_stop_indices = np.argwhere(obs == self.busstop_class)
            # flip the next state to the other side of the stop
            if np.array_equal(next_idx, bus_stop_indices[0]):
                # teleport the agent from bus stop 0 to bus stop 1
                next_idx = bus_stop_indices[1]
            else:
                # teleport the agent from bus stop 1 to bus stop 0
                next_idx = bus_stop_indices[0]

        # if it is Secret Location
        elif obs[tuple(next_idx)] == self.secret_location_class:
            if self.manual_mode:
                print('Central Source Secret Location')
            reward = 300
            # close the shop (remove from the map)
            if self.remove_after_location_found:
                self.initial_observation[tuple(next_idx)] = self.normal_land_class
                self.number_removed_shop += 1
                reward = 9000

        # and the Final Location
        elif obs[tuple(next_idx)] == self.final_location_class:
            if self.manual_mode:
                print('Final Location - well done!')
                print('You have discovered the Final Location of AI center in Trondheim')
            reward = 200


        # now we need to change the observation space
        # i.e. update the environment’s state to reflect the agent's movement

        # if agent’s current position was initial start position
        if self.initial_observation[tuple(start_idx)] == self.start_class:
            # if true, update observation - mark the start position as neutral area
            self.observation[tuple(start_idx)] = self.normal_land_class
        else:
            # if current position not the initial start one
            # restores position in self.observation to its original value from self.initial_observation
            # re-color the observation as where you were if not start position
            self.observation[tuple(start_idx)] = self.initial_observation[tuple(start_idx)]

        # sets the new position of the agent as the next start position
        self.observation[tuple(next_idx)] = self.start_class

        return self.observation, reward, terminated, info


    def reset(self):
        """
        This method resets the environment to its initial values.

        Returns:
            observation:    array
                            the initial state of the environment
        """
        self.number_removed_shop = 0

        img_3d = read_png_file(self.env_image_path, printDebug = False)
        img_2d = make_2d(map_3d=img_3d,
                         palette_array=self.pallete)

        if self.start_random:
          # we need to move the start to another place
          img_2d = swap_values(arr=img_2d,
                               value1=1,
                               value2=2)

        self.observation = img_2d
        self.initial_observation = img_2d.copy()

        max_observation_value = np.max(img_2d)
        observation_space = np.full_like(img_2d,
                                         fill_value=max_observation_value)
        self.observation_space = MultiDiscrete(observation_space)

        # rewards
        self.reward = 0

        # done
        self.done = False

        # terminated
        self.terminated = False

        return self.observation


    def get_1d_state(self):
        """
        This function returns state as 1d observation
        """
        max_x = self.observation.shape[0]
        max_y = self.observation.shape[1]

        start_idx = np.argwhere(
            self.observation == self.start_class)[0]
        state_number = (
            start_idx[0] * max_x) + start_idx[1]
        state_number = (self.number_removed_shop * max_x * max_y) + state_number
        return state_number

    def render(self, mode='human', close=False):
        """
        This methods provides the option to render the environment's behavior to a
        window which should be readable to the human eye if mode is set to 'human'.
        """
        render_ready_arr = make_3d(map_2d=self.observation,
                                   palette_array=self.pallete)
        plt.imshow(render_ready_arr)
        plt.show()
        plt.close()
        return render_ready_arr

    def close(self):
        """
        This method provides the user with the option to perform necessary
        cleanup.
        """
        img_3d = read_png_file(self.env_image_path)
        img_2d = make_2d(map_3d=img_3d,
                         palette_array=self.pallete)
        self.observation = img_2d
        self.initial_observation = img_2d.copy()


# In[ ]:


# let's make sure to create subfolder for our input images, if not already there
input_folder = 'input_images'
if not os.path.exists(input_folder):
    os.makedirs(input_folder)


# # Configuring and loading the environment:

# ## 🔎📉  Let's first visualize our simple toy environment

# In[ ]:


# This is how you create a configuration for the environment
# Note: make sure that folder input_images is created
# Note 2: make sure you copy necessary input images there

env_config = {
    'env_image_path':'input_images/image_simple_1.png',
    'dead_allowed':False,
    'remove_after_location_found':False,
    'start_random':False,
    'number_removable_locations':0,
}


if not os.path.exists(env_config['env_image_path']):
  # make sure you have the folder and the image
  print(f"Error: The image path {env_config['env_image_path']} does not exist.")
else:
  # if everything fine, we create the environment
  tmp_env = TrondheimEnv(conf=env_config)
  print(f"Environment successfully created")


# In[ ]:


# visualize the environment. Yellow is where we are, Black is where we want to go
tmp_env.reset()
tmp_env.render()


# Let's see how actions work:

# In[ ]:


action_dict = {
    'up':0,
    'left':1,
    'down':2,
    'right':3,
}


# In[ ]:


# select your action here
# you are encouraged to play further with this
my_action = 'left'

# now take a step through the environment by taking this action
obs, rew, term, info = tmp_env.step(action_dict[my_action])
print(f"Reward for action {my_action} is: {rew}")
tmp_env.render()

print(f"Observation space looks like this:\n{obs}")


# # 🧠 **Your Assignments**

# # 1️⃣ Assignment 1: Q-learning with simple toy environment

# ## 🐣 Let's learn how to do this in a simple environment
# 
# 
# *   Task 1: Initialize Q table
# *   Task 2: Write necessary helper functions for Q-learning
# *   Task 3: Play with Hyperparameters
# *   Task 4: Train the agent using Q-learning
# 
# **Note**: Task 2, Task 3 and Task 4: you will use the same code you implemented for these in the Assignment 2.

# In[ ]:


# Environment is set up for you here
env_config = {
    'env_image_path':'input_images/image_simple_1.png',
    'dead_allowed':False,
    'remove_after_location_found':False,
    'start_random':False,
    'number_removable_locations':0,
}

if not os.path.exists(env_config['env_image_path']):
  # make sure you have the folder and the image
  print(f"Error: The image path {env_config['env_image_path']} does not exist.")
else:
  # if everything fine, we create the environment
  tmp_env = TrondheimEnv(conf=env_config)
  print(f"Environment successfully created")


# ### Task 1: Initialize Q table

# In[ ]:


# Task 1: Initialize Q table
"""
TODO: given the environment, initialize the Q table
"""

# Tip: pay attention to the dimensions of Q table
# Tip: how many states does the table have, how many actions

print(f"Observation space dimensions:  {tmp_env.observation_space.shape}")
print(f"Number of possible actions: {tmp_env.action_space.n}")

# YOUR CODE GOES HERE
Q_test = None # you need to initialize this variable
#print(Q_test.shape)


# In[ ]:


# take a look into initial Q table (randomized one)
print(f"Q table has this shape: {Q_test.shape}\n")

# you may want to print Q table, or parts of it, to get the vibe
print(Q_test[:10])
print(f"\n")
#print(Q_test[135:])


# ### Task 2: Write necessary helper functions for Q-learning

# In[ ]:


# Task 2: Write necessary helper functions for Q-learning
"""
TODO: write helper function to:
 update_explore_rate() - controls the balance between exploration and exploitation
 update_learning_rate() - controls how aggressively the agent updates its knowledge

 update_action() - uses the exploration rate to decide whether to take a
 random action (explore) or to act according to the learned Q-values (exploit)
"""


# Explore Rate Decay Function, Converges to MIN_EXPLORE_RATE
def update_explore_rate(episode, MIN_EXPLORE_RATE):
    # this function should return updated explore_rate
	# YOUR CODE GOES HERE


# Learning Rate Decay Function, Converges to MIN_LEARNING_RATE
def update_learning_rate(episode, MIN_LEARNING_RATE):
    # this function should return updated learning_rate
	# YOUR CODE GOES HERE


# returns an action based on current state, explore rate and Q table
def update_action(env, state, explore_rate, Q):
    # this function should return an action
	# YOUR CODE GOES HERE


# ### Task 3: Play with Hyperparameters

# In[ ]:


# Task 3: Play with Hyperparameters

# YOUR CODE GOES HERE: replace None with your values
GAMMA_test = None
NUM_EPISODES_test = None
MIN_EXPLORE_RATE_test = None
MIN_LEARNING_RATE_test = None
STEP_SIZE_TEST = None


# ## 🐤 Now use Q learning to learn the Q table
# ### Task 4: Train the agent using Q-learning

# The Q-value update equation, derived from Bellman equation is as follows:
# 

# $$
# Q(s, a) \gets Q(s, a) + \alpha*\Big[ R(s, a) + \gamma* \max_{a'} Q(s', a') - Q(s, a) \Big]
# $$

# **Legend:**
# 
# - **Q(s, a)**: Action-value function, estimating the expected return for taking action *a* in state *s*
# - **α**: learning rate
# - **R(s, a)**: reward received after taking action *a* in state *s*
# - **γ**: discount factor (importance of future rewards)
# - **s'**: Next state resulting from taking action *a* in state *s*  
# - **a'**: Possible actions in the next state *s'*
# - **max Q(s', a')**: Maximum estimated Q value of the next state over all possible actions *a'*
# 

# In[ ]:


# Task 4: Train the agent using Q-learning
# Suggestion: you can wrap the code to learn Q-table in a function,
# which you can also use for Task 4 in Assignment 2

# result of the training is:
#   updated Q table (Q_test)
#   total_reward
#   you are free to add others, if you will

tmp_env.manual_mode = False # disable printouts while training


# initialize some variables
total_reward = 0

explore_rate = update_explore_rate(0, MIN_EXPLORE_RATE=MIN_EXPLORE_RATE_test)
learning_rate = update_learning_rate(0, MIN_LEARNING_RATE=MIN_LEARNING_RATE_test)

# YOUR CODE GOES HERE

# Tip: iterate over number of episodes

    # Tip: iterate over number of steps in each Episode
    # Tip: use helper functions defined above to navigate the environment

        # Tip: use the Q-learning update rule equation - Bellman equation



  # Tip : print variables of interest, like episode, total_reward, explore_rate
  # print(f"Episode :{i}  Total_reward: {total_reward}  Explore rate: {explore_rate}")
  #print("Final Q values: ", Q_test)


# In[ ]:


print(f"Total reward after training: {total_reward}, total Explore rate after training: {explore_rate}")


# ## 🐥 Let's see this in action

# In[ ]:


# now we show one episode
# yellow spot should be able to find black spot here

tmp_env.manual_mode = True # enable messages
STEP_SIZE_TEST_test = 16

observation = tmp_env.reset()
state_0 = tmp_env.get_1d_state()
total_reward = 0

for _ in range(STEP_SIZE_TEST_test):
    tmp_env.render()
    action = update_action(tmp_env, state_0, explore_rate, Q=Q_test)
    obv, reward, done, info = tmp_env.step(action)
    state_1 = tmp_env.get_1d_state()

    # No need to update the Q here
    state_0 = state_1

    total_reward += reward

print(f"Total_reward: ", total_reward)


# In[ ]:


# the shape of Q table (Q_test)
print(Q_test.shape)
print(f"Obs space dimensions :{tmp_env.observation_space.shape}")


# In[ ]:


# print Q_table in "observation space" light
Q_test.argmax(axis=1).reshape(tmp_env.observation_space.shape)


# In[ ]:


# close the environment
tmp_env.close()


# # 2️⃣ Assignment 2: Trondheim treasure hunt!
# 

# # 🐔 You are ready for the Trondheim challenge now 💪
# 
# *   Task 1: Initialize Q table
# *   Task 2: Write necessary helper functions for Q-learning
# *   Task 3: Play with Hyperparameters
# *   Task 4: Train the agent using Q-learning
# 
# 
# ## **Important Note**:
# For Task 2, Task 3 and Task 4, you can (re)use the same code as in the Assignment 1.
# **These are mentioned here as the elements of the pipeline.**
# 
# Most of the code from Assigment 1 is reusable, we are just using it with different environment here.

# In[ ]:


# First and important - cleanup!
tmp_env.close()


# In[ ]:


# let's configure the environment and see how it looks like
env_config = {
    'env_image_path':'input_images/image_TRD_2.png',
    'dead_allowed':False,
    'remove_after_location_found':True,
    'start_random':False,
    'number_removable_locations':1,
}

if not os.path.exists(env_config['env_image_path']):
  # make sure you have the folder and the image
  print(f"Error: The image path {env_config['env_image_path']} does not exist.")
else:
  # if everything fine, we create the environment
  tmp_env = TrondheimEnv(conf=env_config)
  print(f"Environment successfully created")


# In[ ]:


tmp_env.reset()
tmp_env.render()


# In[ ]:


# select the action here
my_action = 'right'

# here is your action
obs, rew, term, info = tmp_env.step(action_dict[my_action])
print(rew)
tmp_env.render()


# ### Task 1: Initialize Q table

# In[ ]:


# Task 1: Initialize Q table
"""
TODO: given the environment, initialize the Q table
"""

# Tip: pay attention to the dimensions of Q table
# Tip: how many states the table have, how many actions
# Extra tip:
"""number of states is different here in comparison with first example:
we have 2 subcases for this environment:
when Location is there and when it is removed/destroyed"""

print(f"Observation space dimensions:  {tmp_env.observation_space.shape}")
print(f"Number of possible actions: {tmp_env.action_space.n}")

# YOUR CODE GOES HERE


Q_test = None # you need to initialize Q_test
#print(Q_test.shape)


# ### Task 2: Write necessary helper functions for Q-learning

# In[ ]:


# Task 2: Write necessary helper functions for Q-learning
# Tip: just use the same helper functions you already implemented above

# YOU DON'T NEED TO DO ANYTHING HERE. They are the same


# ### Task 3: Play with Hyperparameters

# In[ ]:


# Task 3: Play with Hyperparameters
# Tip - you can also use the same parameters as for the toy example above
# YOUR CODE GOES HERE (set the values below)

GAMMA_test = None
NUM_EPISODES_test = None
MIN_EXPLORE_RATE_test = None
MIN_LEARNING_RATE_test = None
STEP_SIZE_TEST = None
# Depending on your parameters for the toy environment, using the same parameters here could work fine
# if they are not working, you may want to set them to different values here


# ## Now use Q-learning to learn the Q table
# ### Task 4: Train the agent using Q-learning

# In[ ]:


# Task 4: Train the agent using Q-learning on
# Tip - you can use the same function (or code) that you implemented for simple environment (Assignment 1 Task 4)

# the logic is the same, just the enviroment is different
# YOUR CODE GOES HERE


# In[ ]:


print(f"Total reward after training: {total_reward}, total Explore rate after training: {explore_rate}")


# ## Let's see one episode

# In[ ]:


#now we can show one episode

tmp_env.manual_mode = True # we want to see the messages
STEP_SIZE_TEST_test = 45

image_samples = []  # Array to store images

observation = tmp_env.reset()
state_0 = tmp_env.get_1d_state()
total_reward = 0

for _ in range(STEP_SIZE_TEST_test):
    img = tmp_env.render()
    image_samples.append(img)

    action = update_action(tmp_env, state_0, explore_rate, Q=Q_test)
    obv, reward, done, info = tmp_env.step(action)
    state_1 = tmp_env.get_1d_state()
    state_0 = state_1

    total_reward += reward

print(f"Total_reward: ", total_reward)
# self.observation.shape[0]


# ### Simple animation of agent movement in one episode

# In[ ]:


# let's make an animation of this, because it's fun
# nothing for you to do here - just run the code

import os
import matplotlib.animation as animation

height = img.shape[0]
width = img.shape[1]

output_folder = 'output_stuff'
if not os.path.exists(output_folder):
    os.makedirs(output_folder)


fig = plt.figure()
imgs = []
for i in range(STEP_SIZE_TEST_test):
    im = plt.imshow(image_samples[i].reshape(height, width, 3), animated=True)
    imgs.append([im])

animate = animation.ArtistAnimation(fig, imgs, interval=80, blit=True, repeat_delay=1000)
filename_gif = os.path.join(output_folder, 'animation_test.gif')
animate.save(filename_gif)
plt.show()

