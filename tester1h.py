# Based on testing harness dated 2017-06-02.

# STUDENTS: TO USE:
# 
# The following command will test all test cases on your file:
# 
#   python3 <thisfile.py> <your_one_file.py>
# 
# 
# You can also limit the tester to only the functions you want tested.
# Just add as many functions as you want tested on to the command line at the end.
# Example: to only run tests associated with func1 and func2, run this command:
# 
#   python3 <thisfile.py> <your_one_file.py> func1 func2
# 
# You really don't need to read the file any further, except that when
# a specific test fails, you'll get a line number - and it's certainly
# worth looking at those areas for details on what's being checked. This would
# all be the indented block of code starting with "class AllTests".


# INSTRUCTOR: TO PREPARE:
#  - add test cases to class AllTests. The test case functions' names must
# be precise - to test a function named foobar, the test must be named "test_foobar_#"
# where # may be any digits at the end, such as "test_foobar_13".
# - any extra-credit tests must be named "test_extra_credit_foobar_#"
# 
# - name all required definitions in REQUIRED_DEFNS, and all extra credit functions
#   in EXTRA_CREDIT_DEFNS. Do not include any unofficial helper functions. If you want
#   to make helper definitions to use while testing, those can also be added there for
#   clarity.
# 
# - to run on either a single file or all .py files in a folder (recursively):
#   python3 <thisfile.py> <your_one_file.py>
#   python3 <thisfile.py> <dir_of_files>
#   python3 <thisfile.py> .                    # current directory
# 
# A work in progress by Mark Snyder, Oct. 2015.
#  Edited by Yutao Zhong, Spring 2016.
#  Edited by Raven Russell, Spring 2017.
#  Edited by Mark Snyder, June 2017.


import unittest
import shutil
import sys
import os
import time

#import subprocess

import importlib

############################################################################
############################################################################
# BEGIN SPECIALIZATION SECTION (the only part you need to modify beyond 
# adding new test cases).

# name all expected definitions; if present, their definition (with correct
# number of arguments) will be used; if not, a decoy complainer function
# will be used, and all tests on that function should fail.
    
REQUIRED_DEFNS = [
                    "coprime",
                    "prime_factors",
                    "trib",
                    "max_two",
                    "reversed",
                    "clockwise",
                    "any",
                    "select",
                    "zip_with",
                    "augdentity",
                 ]

# for method names in classes that will be tested
SUB_DEFNS = [ ]

# definitions that are used for extra credit
EXTRA_CREDIT_DEFNS = [ ]

# how many points are test cases worth?
weight_required = 1
weight_extra_credit = 1

# don't count extra credit; usually 100% if this is graded entirely by tests.
# it's up to you the instructor to do the math and add this up!
# TODO: auto-calculate this based on all possible tests.
total_points_from_tests = 100

# how many seconds to wait between batch-mode gradings? 
# ideally we could enforce python to wait to open or import
# files when the system is ready but we've got a communication
# gap going on.
DELAY_OF_SHAME = 1


# set it to true when you run batch mode... 
CURRENTLY_GRADING = False

# what temporary file name should be used for the student?
# This can't be changed without hardcoding imports below, sorry.
# That's kind of the whole gimmick here that lets us import from
# the command-line argument without having to qualify the names.
RENAMED_FILE = "student"

def same_items(xs,ys):
    if len(xs) != len(ys):
        return False
    for x in xs:
        if x not in ys:
            return False
    return True

def even(n):
    return n%2==0

def odd(n):
    return n%2==1

# END SPECIALIZATION SECTION
############################################################################
############################################################################

# enter batch mode by giving a directory to work on as the only argument.
BATCH_MODE = len(sys.argv)==2 and (sys.argv[1] in ["."] or os.path.isdir(sys.argv[1]))

# This class contains multiple "unit tests" that each check
# various inputs to specific functions, checking that we get
# the correct behavior (output value) from completing the call.
class AllTests (unittest.TestCase):
        
    ############################################################################
    
    # --------------------------------------------------------------------
    
    # counts
    def test_prime_factors_01(self): self.assertEqual(prime_factors(1),[])
    def test_prime_factors_02(self):
        self.assertEqual(prime_factors(2),[2])
        self.assertEqual(prime_factors(3),[3])
    def test_prime_factors_03(self): self.assertEqual(prime_factors(4),[2,2])
    def test_prime_factors_04(self): self.assertEqual(prime_factors(5),[5])
    def test_prime_factors_05(self): self.assertEqual(prime_factors(50),[2,5,5])
    def test_prime_factors_06(self): self.assertEqual(prime_factors(66),[2,3,11])
    def test_prime_factors_07(self): self.assertEqual(prime_factors(100),[2,2,5,5])
    def test_prime_factors_08(self): self.assertEqual(prime_factors(463),[463])
    def test_prime_factors_09(self): self.assertEqual(prime_factors(512),[2,2,2,2,2,2,2,2,2])
    def test_prime_factors_10(self):         
        self.assertEqual(prime_factors(117),[3,3,13])
        self.assertEqual(prime_factors(1117),[1117])
    def test_prime_factors_11(self): self.assertEqual(prime_factors(44100),[2,2,3,3,5,5,7,7])

    # --------------------------------------------------------------------
    
    def test_coprime_01(self):
        self.assertEqual(coprime(  2,   3), True  )
        self.assertEqual(coprime(  3,   7), True  )
    def test_coprime_02(self): 
        self.assertEqual(coprime(  3,  10), True  )
        self.assertEqual(coprime(  5,  10), False )
    def test_coprime_03(self): self.assertEqual(coprime( 10,  15), False )
    def test_coprime_04(self): self.assertEqual(coprime( 15,  10), False )
    def test_coprime_05(self): self.assertEqual(coprime( 50,  61), True  )
    def test_coprime_06(self): self.assertEqual(coprime(100, 200), False )
    def test_coprime_07(self): self.assertEqual(coprime( 97,  98), True  )
    def test_coprime_08(self): self.assertEqual(coprime( 66, 201), False )
    def test_coprime_09(self): self.assertEqual(coprime( 49,  28), False )
    def test_coprime_10(self):
        self.assertEqual(coprime(367, 463), True  )
        self.assertEqual(coprime(330, 463), True  )
    def test_coprime_11(self): self.assertEqual(coprime(867,5309), True  )
    
    # --------------------------------------------------------------------
    
    def test_trib_01(self): self.assertEqual(trib(0),1)
    def test_trib_02(self): self.assertEqual(trib(2),1)
    def test_trib_03(self): self.assertEqual(trib(3),3)
    def test_trib_04(self): self.assertEqual(trib(4),5)
    def test_trib_05(self): self.assertEqual(trib(5),9)
    def test_trib_06(self): self.assertEqual(trib(6),17)
    def test_trib_07(self): self.assertEqual(trib(10),193)
    def test_trib_08(self): self.assertEqual(trib(12),653)
    def test_trib_09(self): self.assertEqual(trib(13),1201)
    def test_trib_10(self):
        self.assertEqual(trib(20),85525)
        self.assertEqual(trib(30),37895489)
    def test_trib_11(self): self.assertEqual(trib(35),797691075)
    
    # --------------------------------------------------------------------
    
    def test_max_two_01(self):  self.assertEqual(max_two([1,2,3,4,5,1,2]),[5,4])
    def test_max_two_02(self):  self.assertEqual(max_two([3,6,1,2,6,4]),[6,6])
    def test_max_two_03(self):  self.assertEqual(max_two([-3,-4,-5,-2,-10]),[-2,-3])
    def test_max_two_04(self):  self.assertEqual(max_two([5]),[5])
    def test_max_two_05(self):  self.assertEqual(max_two([]),[])
    def test_max_two_06(self):  self.assertEqual(max_two([-1,-2,50,-3,14,0,-10]),[50,14])
    def test_max_two_07(self):  self.assertEqual(max_two([-3,-4,-5,-3,-4,-5]),[-3,-3])
    def test_max_two_08(self):  self.assertEqual(max_two([1,0,-1]),[1,0])
    def test_max_two_09(self):  self.assertEqual(max_two([-(2**1000)]),[-2**1000])
    def test_max_two_10(self):
        arg = [1,4,2,6,5,23,4,5,12,15]
        orig = arg[:]
        ans = max_two(arg)
        self.assertEqual(ans, [23,15])
        # must preserve original list.
        self.assertEqual(orig, arg)
    def test_max_two_11(self):  self.assertEqual(max_two([-1,0,-2]),[0,-1])
    
    # --------------------------------------------------------------------
    
    def test_reversed_01(self):  self.assertEqual(reversed([]),[])
    def test_reversed_02(self):  self.assertEqual(reversed([5]),[5])
    def test_reversed_03(self):  self.assertEqual(reversed([2,6]),[6,2])
    def test_reversed_04(self):  self.assertEqual(reversed([4,6,3]),[3,6,4])
    def test_reversed_05(self):  self.assertEqual(reversed(["hello",5,[1,2,3]]),[[1,2,3],5,"hello"])
    def test_reversed_06(self):  self.assertEqual(reversed(list(range(0,1000+1))),list(range(1000,0-1,-1)))
    def test_reversed_07(self):  self.assertEqual(reversed([-1,3,0,-2,4]),[4,-2,0,3,-1])
    def test_reversed_08(self):
        orig = [1,2,3]
        arg = orig[:] # copy
        self.assertEqual(reversed(arg),[3,2,1])
        self.assertEqual(orig,arg)
    def test_reversed_09(self):
        orig = [5,2,4,1,3]
        arg = orig[:] # copy
        self.assertEqual(reversed(arg),[3,1,4,2,5])
        self.assertEqual(orig,arg)
    def test_reversed_10(self):
        orig = [1,2,3,8,7,6,4,5,6,9,9,9]
        arg = orig[:] # copy
        self.assertEqual(reversed(arg),[9,9,9,6,5,4,6,7,8,3,2,1])
        self.assertEqual(orig,arg)
    def test_reversed_11(self):  self.assertEqual(reversed([1,2,3,5,4]),[4,5,3,2,1])
    
    # --------------------------------------------------------------------
    
    def test_clockwise_01(self):  self.assertEqual(clockwise([[1,2],[3,4]]),[[3,1],[4,2]])
    def test_clockwise_02(self):  self.assertEqual(clockwise([[5]]),[[5]])
    def test_clockwise_03(self):  self.assertEqual(clockwise([]),[])
    def test_clockwise_04(self):  self.assertEqual(clockwise([[1,2,3],[4,5,6]]),[[4, 1], [5, 2], [6, 3]])
    def test_clockwise_05(self):  self.assertEqual(clockwise([[1,2],[3,4],[5,6]]),[[5,3,1],[6,4,2]])
    def test_clockwise_06(self):  self.assertEqual(clockwise([[1,2,3,4,5]]),[[1], [2], [3], [4], [5]])
    def test_clockwise_07(self):  self.assertEqual(clockwise([[1], [2], [3], [4], [5]]),[[5, 4, 3, 2, 1]])
    def test_clockwise_08(self):
        try:
            clockwise([[1,2,3],[1,2,3,4,5,6]])
        except ValueError as e:
            self.assertEqual(str(e), "not rectangular")
            return
        self.fail("should have raised ValueError because the input wasn't rectangular.")
    def test_clockwise_09(self):  self.assertEqual(clockwise([[{1},{2}],[True,"mixed"]]),[[True,{1}],["mixed",{2}]])
    def test_clockwise_10(self):
        orig = [[1,2,3],[4,5,6]]
        arg  = [[1,2,3],[4,5,6]]
        ans = clockwise(arg)
        self.assertEqual(ans,[[4, 1], [5, 2], [6, 3]])
        self.assertEqual(orig, arg)
    
    # --------------------------------------------------------------------
    
    def test_any_01(self):  self.assertEqual(any([]),False)
    def test_any_02(self):  self.assertEqual(any([False]),False)
    def test_any_03(self):  self.assertEqual(any([True]), True)
    def test_any_04(self):  self.assertEqual(any([True,False]),True)
    def test_any_05(self):  self.assertEqual(any([True, False, True]),True)
    def test_any_06(self):  self.assertEqual(any([True, True, True, True, True]),True)
    def test_any_07(self):  self.assertEqual(any([False, True, True, False]),True)
    def test_any_08(self):  self.assertEqual(any([False, False, False, False, False]),False)
    def test_any_09(self):
        orig = [False, False, True, True]
        arg = orig[:]
        self.assertEqual(any(arg),True)
        self.assertEqual(orig, arg)
    def test_any_10(self):
        orig = [False, False, False]
        arg = orig[:]
        self.assertEqual(any(arg),False)
        self.assertEqual(orig, arg)
    
    # --------------------------------------------------------------------
        
    def test_select_01(self):  self.assertEqual(select(even,[1,2,3,4,5]),[2,4])
    def test_select_02(self):  self.assertEqual(select(odd, [1,2,3,4,5]),[1,3,5])
    def test_select_03(self):
        def window(x):
            return 5 < x < 10
        self.assertEqual(select(window, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]),[6,7,8,9])
    
    def test_select_04(self):  self.assertEqual(select(lambda x: coprime(x,5), [4,5,6,7,8,9,10,11,12]   ),[4,6,7,8,9,11,12])
    def test_select_05(self):  self.assertEqual(select(all, [[True, False, True], [True], [True,True],[False],[True]]),[[True],[True,True],[True]])
    def test_select_06(self):  self.assertEqual(select(lambda x: x==5,[7,6,5,4,3,4,5,6,7]),[5,5])
    def test_select_07(self):  self.assertEqual(select(even,[1,3,5,7,9]),[])
    def test_select_08(self):  self.assertEqual(select(even,[]),[])
    def test_select_09(self):  self.assertEqual(select(even,[10,100]),[10,100])
    def test_select_10(self):
        orig = [3,4,5,6,7,6,5,4,3]
        arg = orig[:]
        self.assertEqual(select(even,arg),[4,6,6,4])
        self.assertEqual(orig, arg)
    
    # --------------------------------------------------------------------
    
    def test_zip_with_01(self):
        def add(x,y): return x+y
        self.assertEqual(zip_with(add, [1,2,3,4], [10,10,10,10]),[11,12,13,14])
    def test_zip_with_02(self):
        def add(x,y): return x+y
        self.assertEqual(zip_with(add, [1,2,3,4], [5,6,7,8]),[6,8,10,12])
    def test_zip_with_03(self):
        def mul(x,y): return x*y
        # first list has fewer elements.
        self.assertEqual(zip_with(mul, [2,3,4],  [5,5,5,5,5]), [10,15,20])
    def test_zip_with_04(self):
        def mul(x,y): return x*y
        # second list has fewer elements.
        self.assertEqual(zip_with(mul, [2,3,4,5,6,7,8],  [5,5,5]), [10,15,20])
        def mul(x,y): return x*y
        # first list has zero elements.
        self.assertEqual(zip_with(mul, [],  [5,5,5,5,5]), [])
    def test_zip_with_05(self):
        def mul(x,y): return x*y
        # second list has zero elements.
        self.assertEqual(zip_with(mul, [2,3,4,5,6,7,8],  []), [])
    def test_zip_with_06(self):
        def mul(x,y): return x*y
        # both lists have zero elements.
        self.assertEqual(zip_with(mul, [],  []), [])
    
    # double-weighting some tests.
    def test_zip_with_07(self):  self.test_zip_with_01()
    def test_zip_with_08(self):  self.test_zip_with_02()
    def test_zip_with_09(self):  self.test_zip_with_03()
    def test_zip_with_10(self): self.test_zip_with_04()
    
    # --------------------------------------------------------------------
    
    def test_augdentity_01(self): self.assertEqual(augdentity(2,2), [[1,0],[0,1]])
    def test_augdentity_02(self): self.assertEqual(augdentity(2,3), [[1,0,0],[0,1,0]])
    def test_augdentity_03(self): self.assertEqual(augdentity(1,1), [[1]])
    def test_augdentity_04(self): self.assertEqual(augdentity(3,2), [[1,0],[0,1],[0,0]])
    def test_augdentity_05(self): self.assertEqual(augdentity(3,3), [[1,0,0],[0,1,0],[0,0,1]])
    def test_augdentity_06(self): self.assertEqual(augdentity(3,5), [[1,0,0,0,0],[0,1,0,0,0],[0,0,1,0,0]])
    def test_augdentity_07(self): self.assertEqual(augdentity(5,2), [[1,0],[0,1],[0,0],[0,0],[0,0]])
    def test_augdentity_08(self): self.assertEqual(augdentity(1,6), [[1,0,0,0,0,0]])
    def test_augdentity_09(self):
        ans = augdentity(3,3)
        expected = [[1,0,0],[0,1,0],[0,0,1]]
        self.assertEqual(expected, ans)
        # modify stuff and see if any inadvertent aliasing is exposed
        ans[1][1] = 100
        ans[2][0] = 999
        self.assertEqual(ans, [[1,0,0],[0,100,0],[999,0,1]])
        
    def test_augdentity_10(self):
        ans = augdentity(3,5)
        expected = [[1,0,0,0,0],[0,1,0,0,0],[0,0,1,0,0]]
        self.assertEqual(expected, ans)
        # modify stuff and see if any inadvertent aliasing is exposed
        ans[1][1] = 100
        ans[2][4] = 999
        self.assertEqual(ans, [[1,0,0,0,0],[0,100,0,0,0],[0,0,1,0,999]])
        
    
    # --------------------------------------------------------------------
    ############################################################################
    
# This class digs through AllTests, counts and builds all the tests,
# so that we have an entire test suite that can be run as a group.
class TheTestSuite (unittest.TestSuite):
    # constructor.
    def __init__(self,wants):
        self.num_req = 0
        self.num_ec = 0
        # find all methods that begin with "test".
        fs = []
        for w in wants:
            for func in AllTests.__dict__:
                # append regular tests
                # drop any digits from the end of str(func).
                dropnum = str(func)
                while dropnum[-1] in "1234567890":
                    dropnum = dropnum[:-1]
                
                if dropnum==("test_"+w+"_") and (not (dropnum==("test_extra_credit_"+w+"_"))):
                    fs.append(AllTests(str(func)))
                if dropnum==("test_extra_credit_"+w+"_") and not BATCH_MODE:
                    fs.append(AllTests(str(func)))
        
#       print("TTS ====> ",list(map(lambda f: (f,id(f)),fs)))
        # call parent class's constructor.
        unittest.TestSuite.__init__(self,fs)

class TheExtraCreditTestSuite (unittest.TestSuite):
        # constructor.
        def __init__(self,wants):
            # find all methods that begin with "test_extra_credit_".
            fs = []
            for w in wants:
                for func in AllTests.__dict__:
                    if str(func).startswith("test_extra_credit_"+w):
                        fs.append(AllTests(str(func)))
        
#           print("TTS ====> ",list(map(lambda f: (f,id(f)),fs)))
            # call parent class's constructor.
            unittest.TestSuite.__init__(self,fs)

# all (non-directory) file names, regardless of folder depth,
# under the given directory 'dir'.
def files_list(dir):
    this_file = __file__
    if dir==".":
        dir = os.getcwd()
    info = os.walk(dir)
    filenames = []
    for (dirpath,dirnames,filez) in info:
#       print(dirpath,dirnames,filez)
        if dirpath==".":
            continue
        for file in filez:
            if file==this_file:
                continue
            filenames.append(os.path.join(dirpath,file))
#       print(dirpath,dirnames,filez,"\n")
    return filenames

def main():
    if len(sys.argv)<2:
        raise Exception("needed student's file name as command-line argument:"\
            +"\n\t\"python3 testerX.py gmason76_2xx_Px.py\"")
    
    if BATCH_MODE:
        print("BATCH MODE.\n")
        run_all()
        return
        
    else:
        want_all = len(sys.argv) <=2
        wants = []
        # remove batch_mode signifiers from want-candidates.
        want_candidates = sys.argv[2:]
        for i in range(len(want_candidates)-1,-1,-1):
            if want_candidates[i] in ['.'] or os.path.isdir(want_candidates[i]):
                del want_candidates[i]
    
        # set wants and extra_credits to either be the lists of things they want, or all of them when unspecified.
        wants = []
        extra_credits = []
        if not want_all:
            for w in want_candidates:
                if w in REQUIRED_DEFNS:
                    wants.append(w)
                elif w in SUB_DEFNS:
                    wants.append(w)
                elif w in EXTRA_CREDIT_DEFNS:
                    extra_credits.append(w)
                else:
                    raise Exception("asked to limit testing to unknown function '%s'."%w)
        else:
            wants = REQUIRED_DEFNS + SUB_DEFNS
            extra_credits = EXTRA_CREDIT_DEFNS
        
        # now that we have parsed the function names to test, run this one file.    
        run_one(wants,extra_credits)    
        return
    return # should be unreachable! 

# only used for non-batch mode, since it does the printing.
# it nicely prints less info when no extra credit was attempted.
def run_one(wants, extra_credits):
    
    has_reqs = len(wants)>0
    has_ec   = len(extra_credits)>0
    
    # make sure they exist.
    passed1 = 0
    passed2 = 0
    tried1 = 0
    tried2 = 0
    
    # only run tests if needed.
    if has_reqs:
        print("\nRunning required definitions:")
        (tag, passed1,tried1) = run_file(sys.argv[1],wants,False)
    if has_ec:
        print("\nRunning extra credit definitions:")
        (tag, passed2,tried2) = run_file(sys.argv[1],extra_credits,True)
    
    # print output based on what we ran.
    if has_reqs and not has_ec:
        print("\n%d/%d Required test cases passed (worth %d each)" % (passed1,tried1,weight_required) )
        print("\nScore based on test cases: %.2f/%d (%.2f*%d) " % (
                                                                passed1*weight_required, 
                                                                total_points_from_tests,
                                                                passed1,
                                                                weight_required
                                                             ))
    elif has_ec and not has_reqs:
        print("%d/%d Extra credit test cases passed (worth %d each)" % (passed2, tried2, weight_extra_credit))
    else: # has both, we assume.
        print("\n%d / %d Required test cases passed (worth %d each)" % (passed1,tried1,weight_required) )
        print("%d / %d Extra credit test cases passed (worth %d each)" % (passed2, tried2, weight_extra_credit))
        print("\nScore based on test cases: %.2f / %d ( %d * %d + %d * %d) " % (
                                                                passed1*weight_required+passed2*weight_extra_credit, 
                                                                total_points_from_tests,
                                                                passed1,
                                                                weight_required,
                                                                passed2,
                                                                weight_extra_credit
                                                             ))
    if CURRENTLY_GRADING:
        print("( %d %d %d %d )\n%s" % (passed1,tried1,passed2,tried2,tag))

# only used for batch mode.
def run_all():
        filenames = files_list(sys.argv[1])
        #print(filenames)
        
        wants = REQUIRED_DEFNS + SUB_DEFNS
        extra_credits = EXTRA_CREDIT_DEFNS
        
        results = []
        for filename in filenames:
            print(" Batching on : " +filename)
            # I'd like to use subprocess here, but I can't get it to give me the output when there's an error code returned... TODO for sure.
            lines = os.popen("python3 tester1p.py \""+filename+"\"").readlines()
            
            # delay of shame...
            time.sleep(DELAY_OF_SHAME)
            
            name = os.path.basename(lines[-1])
            stuff =lines[-2].split(" ")[1:-1]
            print("STUFF: ",stuff, "LINES: ", lines)
            (passed_req, tried_req, passed_ec, tried_ec) = stuff
            results.append((lines[-1],int(passed_req), int(tried_req), int(passed_ec), int(tried_ec)))
            continue
        
        print("\n\n\nGRAND RESULTS:\n")
        
            
        for (tag_req, passed_req, tried_req, passed_ec, tried_ec) in results:
            name = os.path.basename(tag_req).strip()
            earned   = passed_req*weight_required + passed_ec*weight_extra_credit
            possible = tried_req *weight_required # + tried_ec *weight_extra_credit
            print("%10s : %3d / %3d = %5.2d %% (%d/%d*%d + %d/%d*%d)" % (
                                                            name,
                                                            earned,
                                                            possible, 
                                                            (earned/possible)*100,
                                                            passed_req,tried_req,weight_required,
                                                            passed_ec,tried_ec,weight_extra_credit
                                                          ))
# only used for batch mode.
def run_all_orig():
        filenames = files_list(sys.argv[1])
        #print(filenames)
        
        wants = REQUIRED_DEFNS + SUB_DEFNS
        extra_credits = EXTRA_CREDIT_DEFNS
        
        results = []
        for filename in filenames:
            # wipe out all definitions between users.
            for fn in REQUIRED_DEFNS+EXTRA_CREDIT_DEFNS :
                globals()[fn] = decoy(fn)
                fn = decoy(fn)
            try:
                name = os.path.basename(filename)
                print("\n\n\nRUNNING: "+name)
                (tag_req, passed_req, tried_req) = run_file(filename,wants,False)
                (tag_ec,  passed_ec,  tried_ec ) = run_file(filename,extra_credits,True)
                results.append((tag_req,passed_req,tried_req,tag_ec,passed_ec,tried_ec))
                print(" ###### ", results)
            except SyntaxError as e:
                tag = filename+"_SYNTAX_ERROR"
                results.append((tag,0,len(wants),tag,0,len(extra_credits)))
            except NameError as e:
                tag =filename+"_Name_ERROR"
                results.append((tag,0,len(wants),tag,0,len(extra_credits)))
            except ValueError as e:
                tag = filename+"_VALUE_ERROR"
                results.append((tag,0,len(wants),tag,0,len(extra_credits)))
            except TypeError as e:
                tag = filename+"_TYPE_ERROR"
                results.append((tag,0,len(wants),tag,0,len(extra_credits)))
            except ImportError as e:
                tag = filename+"_IMPORT_ERROR_TRY_AGAIN"
                results.append((tag,0,len(wants),tag,0,len(extra_credits)))
            except Exception as e:
                tag = filename+str(e.__reduce__()[0])
                results.append((tag,0,len(wants),tag,0,len(extra_credits)))
        
#           try:
#               print("\n |||||||||| scrupe: "+str(scruples))
#           except Exception as e:
#               print("NO SCRUPE.",e)
#           scruples = None
        
        print("\n\n\nGRAND RESULTS:\n")
        for (tag_req, passed_req, tried_req, tag_ec, passed_ec, tried_ec) in results:
            name = os.path.basename(tag_req)
            earned   = passed_req*weight_required + passed_ec*weight_extra_credit
            possible = tried_req *weight_required # + tried_ec *weight_extra_credit
            print("%10s : %3d / %3d = %5.2d %% (%d/%d*%d + %d/%d*%d)" % (
                                                            name,
                                                            earned,
                                                            possible, 
                                                            (earned/possible)*100,
                                                            passed_req,tried_req,weight_required,
                                                            passed_ec,tried_ec,weight_extra_credit
                                                          ))

def try_copy(filename1, filename2, numTries):
    have_copy = False
    i = 0
    while (not have_copy) and (i < numTries):
        try:
            # move the student's code to a valid file.
            shutil.copy(filename1,filename2)
            
            # wait for file I/O to catch up...
            if(not wait_for_access(filename2, numTries)):
                return False
                
            have_copy = True
        except PermissionError:
            print("Trying to copy "+filename1+", may be locked...")
            i += 1
            time.sleep(1)
        except BaseException as e:
            print("\n\n\n\n\n\ntry-copy saw: "+e)
    
    if(i == numTries):
        return False
    return True

def try_remove(filename, numTries):
    removed = False
    i = 0
    while os.path.exists(filename) and (not removed) and (i < numTries):
        try:
            os.remove(filename)
            removed = True
        except OSError:
            print("Trying to remove "+filename+", may be locked...")
            i += 1
            time.sleep(1)
    if(i == numTries):
        return False
    return True

def wait_for_access(filename, numTries):
    i = 0
    while (not os.path.exists(filename) or not os.access(filename, os.R_OK)) and i < numTries:
        print("Waiting for access to "+filename+", may be locked...")
        time.sleep(1)
        i += 1
    if(i == numTries):
        return False
    return True

# this will group all the tests together, prepare them as 
# a test suite, and run them.
def run_file(filename,wants=None,checking_ec = False):
    if wants==None:
        wants = []
    
    # move the student's code to a valid file.
    if(not try_copy(filename,"student.py", 5)):
        print("Failed to copy " + filename + " to student.py.")
        quit()
        
    # import student's code, and *only* copy over the expected functions
    # for later use.
    import importlib
    count = 0
    while True:
        try:
#           print("\n\n\nbegin attempt:")
            while True:
                try:
                    f = open("student.py","a")
                    f.close()
                    break
                except:
                    pass
#           print ("\n\nSUCCESS!")
                
            import student
            importlib.reload(student)
            break
        except ImportError as e:
            print("import error getting student... trying again. "+os.getcwd(), os.path.exists("student.py"),e)
            time.sleep(0.5)
            while not os.path.exists("student.py"):
                time.sleep(0.5)
            count+=1
            if count>3:
                raise ImportError("too many attempts at importing!")
        except SyntaxError as e:
            print("SyntaxError in "+filename+":\n"+str(e))
            print("Run your file without the tester to see the details")
            return(filename+"_SYNTAX_ERROR",None, None, None)
        except NameError as e:
            print("NameError in "+filename+":\n"+str(e))
            print("Run your file without the tester to see the details")
            return((filename+"_Name_ERROR",0,1))    
        except ValueError as e:
            print("ValueError in "+filename+":\n"+str(e))
            print("Run your file without the tester to see the details")
            return(filename+"_VALUE_ERROR",0,1)
        except TypeError as e:
            print("TypeError in "+filename+":\n"+str(e))
            print("Run your file without the tester to see the details")
            return(filename+"_TYPE_ERROR",0,1)
        except ImportError as e:            
            print("ImportError in "+filename+":\n"+str(e))
            print("Run your file without the tester to see the details or try again")
            return((filename+"_IMPORT_ERROR_TRY_AGAIN   ",0,1)) 
        except Exception as e:
            print("Exception in loading"+filename+":\n"+str(e))
            print("Run your file without the tester to see the details")
            return(filename+str(e.__reduce__()[0]),0,1)
    
    # make a global for each expected definition.
    for fn in REQUIRED_DEFNS+EXTRA_CREDIT_DEFNS :
        globals()[fn] = decoy(fn)
        try:
            globals()[fn] = getattr(student,fn)
        except:
            if fn in wants:
                print("\nNO DEFINITION FOR '%s'." % fn) 
    
    if not checking_ec:
        # create an object that can run tests.
        runner = unittest.TextTestRunner()
    
        # define the suite of tests that should be run.
        suite = TheTestSuite(wants)
    
    
        # let the runner run the suite of tests.
        ans = runner.run(suite)
        num_errors   = len(ans.__dict__['errors'])
        num_failures = len(ans.__dict__['failures'])
        num_tests    = ans.__dict__['testsRun']
        num_passed   = num_tests - num_errors - num_failures
        # print(ans)
    
    else:
        # do the same for the extra credit.
        runner = unittest.TextTestRunner()
        suite = TheExtraCreditTestSuite(wants)
        ans = runner.run(suite)
        num_errors   = len(ans.__dict__['errors'])
        num_failures = len(ans.__dict__['failures'])
        num_tests    = ans.__dict__['testsRun']
        num_passed   = num_tests - num_errors - num_failures
        #print(ans)
    
    # remove our temporary file.
    os.remove("student.py")
    if os.path.exists("__pycache__"):
        shutil.rmtree("__pycache__")
    if(not try_remove("student.py", 5)):
        print("Failed to remove " + filename + " to student.py.")
    
    tag = ".".join(filename.split(".")[:-1])
    
    
    return (tag, num_passed, num_tests)


# make a global for each expected definition.
def decoy(name):
        # this can accept any kind/amount of args, and will print a helpful message.
        def failyfail(*args, **kwargs):
            return ("<no '%s' definition was found - missing, or typo perhaps?>" % name)
        return failyfail

# this determines if we were imported (not __main__) or not;
# when we are the one file being run, perform the tests! :)
if __name__ == "__main__":
    main()