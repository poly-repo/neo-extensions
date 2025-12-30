;;; -*- lexical-binding: t -*-

;;; This is leetcode, a NEO extension
;;;
;;; Leetcode client and problem manager

(defvar neo--leetcode-branch (neo/git-branch-from-issue "xxx" "leetcode playground"))
(defvar neo--leetcode-directory (expand-file-name
 				 (replace-regexp-in-string "/" "-" neo--leetcode-branch)
 				 "~/.local/share/wtrees/"))

(defvar neo/leetcode-local-mode t)

(defvar neo--leetcode-bind75
  '(
    (1 . "Two Sum")
    (3 . "Longest Substring Without Repeating Characters")
    (5 . "Longest Palindromic Substring")
    (11 . "Container With Most Water")
    (15 . "3Sum")
    (19 . "Remove Nth Node From End of List")
    (20 . "Valid Parentheses")
    (21 . "Merge Two Sorted Lists")
    (23 . "Merge k Sorted Lists")
    (33 . "Search in Rotated Sorted Array")
    (39 . "Combination Sum")
    (48 . "Rotate Image")
    (49 . "Group Anagrams")
    (53 . "Maximum Subarray")
    (54 . "Spiral Matrix")
    (55 . "Jump Game")
    (56 . "Merge Intervals")
    (57 . "Insert Interval")
    (62 . "Unique Paths")
    (70 . "Climbing Stairs")
    (73 . "Set Matrix Zeroes")
    (76 . "Minimum Window Substring")
    (79 . "Word Search")
    (91 . "Decode Ways")
    (98 . "Validate Binary Search Tree")
    (100 . "Same Tree")
    (102 . "Binary Tree Level Order Traversal")
    (104 . "Maximum Depth of Binary Tree")
    (105 . "Construct Binary Tree from Preorder and Inorder Traversal")
    (121 . "Best Time to Buy and Sell Stock")
    (124 . "Binary Tree Maximum Path Sum")
    (125 . "Valid Palindrome")
    (128 . "Longest Consecutive Sequence")
    (133 . "Clone Graph")
    (139 . "Word Break")
    (141 . "Linked List Cycle")
    (143 . "Reorder List")
    (152 . "Maximum Product Subarray")
    (153 . "Find Minimum in Rotated Sorted Array")
    (190 . "Reverse Bits")
    (191 . "Number of 1 Bits")
    (198 . "House Robber")
    (200 . "Number of Islands")
    (206 . "Reverse Linked List")
    (207 . "Course Schedule")
    (208 . "Implement Trie (Prefix Tree)")
    (211 . "Design Add and Search Words Data Structure")
    (212 . "Word Search II")
    (213 . "House Robber II")
    (217 . "Contains Duplicate")
    (226 . "Invert Binary Tree")
    (230 . "Kth Smallest Element in a BST")
    (235 . "Lowest Common Ancestor of a Binary Search Tree")
    (238 . "Product of Array Except Self")
    (242 . "Valid Anagram")
    (252 . "Meeting Rooms")
    (253 . "Meeting Rooms II")
    (261 . "Graph Valid Tree")
    (268 . "Missing Number")
    (269 . "Alien Dictionary")
    (271 . "Encode and Decode Strings")
    (295 . "Find Median from Data Stream")
    (297 . "Serialize and Deserialize Binary Tree")
    (300 . "Longest Increasing Subsequence")
    (322 . "Coin Change")
    (323 . "Number of Connected Components in an Undirected Graph")
    (338 . "Counting Bits")
    (347 . "Top K Frequent Elements")
    (371 . "Sum of Two Integers")
    (417 . "Pacific Atlantic Water Flow")
    (424 . "Longest Repeating Character Replacement")
    (435 . "Non-overlapping Intervals")
    (572 . "Subtree of Another Tree")
    (647 . "Palindromic Substrings")
    (1143 . "Longest Common Subsequence")))

(neo/use-package leetcode
  :custom
  (leetcode-directory (expand-file-name "interview/leetcode" neo--leetcode-directory))
  (leetcode-save-solutions t)
  (leetcode-focus nil)			; we do our thing with projects and perspectives
  ;; TODO create a leetcode environment when this extension is selected
  (leetcode-python-environment (file-name-concat user-emacs-directory ".python")))

(defun neo/leetcode ()
  (interactive)
  (let* ((leetcode-focus t)
	 (original-project (projectile-project-root)))
    (neo/magit-worktree-create neo--leetcode-directory neo--leetcode-branch)
    (neo/switch-to-project neo--leetcode-directory)
    (define-key leetcode--problems-mode-map "q"
		(lambda ()
		  (interactive)
		  (leetcode-quit)
		  (neo/switch-to-project original-project)))
    (leetcode)))

(with-eval-after-load 'leetcode
  (defun leetcode--problems-rows ()
    "Generate tabulated list rows from `leetcode--problems'.
Return a list of rows, each row is a vector:
\([<checkmark> <position> <title> <acceptance> <difficulty>] ...)"
    (let ((problems (leetcode-problems-problems leetcode--problems))
          rows)
      (dolist (p problems (reverse rows))
	(if (or leetcode--display-paid (not (leetcode-problem-paid-only p)))
            (let* ((p-status (if (equal (leetcode-problem-status p) "ac")
				 (leetcode--add-font-lock leetcode--checkmark 'leetcode-checkmark-face)
                               " "))
                   (p-id (leetcode-problem-id p))
                   (p-title (concat
                             (leetcode-problem-title p)
                             " "
                             (if (leetcode-problem-paid-only p)
				 (leetcode--add-font-lock leetcode--paid 'leetcode-paid-face)
                               " ")))
                   (p-acceptance (leetcode-problem-acceptance p))
                   (p-difficulty (leetcode--stringify-difficulty (leetcode-problem-difficulty p)))
                   (p-tags (if leetcode--display-tags (string-join (append (leetcode-problem-tags p) "leet75") ", ") ""))
                   (single-row (vector p-status p-id p-title p-acceptance p-difficulty p-tags)))
              (setq rows (cons single-row rows))))))))


;;; Note, no (provide 'neo-leetcode) here, extensions are loaded not required.
