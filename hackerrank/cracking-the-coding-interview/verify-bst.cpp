bool allGT(Node* root, int val) {
  return (root == NULL) || 
    ((root->data > val) &&
     allGT(root->right, val) &&
     allGT(root->left, val));
}

bool allLT(Node* root, int val) {
  return (root == NULL) || 
    ((root->data < val) &&
     allLT(root->right, val) &&
     allLT(root->left, val));
}

bool checkBST(Node* root) {
  return
    allGT(root->right, root->data) &&
    allLT(root->left, root->data) && 
    ((root->left == NULL) || checkBST(root->left)) && 
    ((root->right == NULL) || checkBST(root->right));
}
