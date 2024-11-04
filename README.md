# Pixel Print Studio - Image Layer Management System

## Overview
Pixel Print Studio is a console application developed in Fortran that allows users to manage and create layered images. The system provides functionality for user management, image creation through layers, and album organization.

## Core Features

### User Management
- User registration with:
  - Full Name
  - DPI (Personal ID)
  - Password
- Login system
- Admin access (username: "admin", password: "EDD2024")
- User data persistence during application runtime

### Layer Management
- Layers stored in Binary Search Trees (BST)
- Each layer contains pixel information in a sparse matrix
- Pixels stored with hexadecimal color values
- Unique layer IDs
- White color displayed where no pixel information exists

### Image Generation
Images can be created in three ways:
1. **Limited Traversal**
   - Select number of layers
   - Choose traversal type: Preorder, Inorder, Postorder
   - Layers stack based on traversal order

2. **Image Tree**
   - Search by image ID
   - Generate using breadth-first traversal of layer tree

3. **Layer Selection**
   - Manual selection of one or more layers
   - Stack selected layers to form image

### Album Organization
- Each user has a list of albums
- Albums contain linked lists of images
- Images reference the main AVL tree storage

## Data Structures

The system implements the following custom data structures:
- **B-Tree (Order 5)**: For client/user storage
- **Sparse Matrix**: For layer pixel information
- **Binary Search Tree (BST)**: For layer storage
- **AVL Tree**: For image storage
- **Doubly Linked Circular List**: For album management

## Bulk Loading
The system supports bulk loading in the following order:
1. Clients (Admin only)
2. Layers (Logged-in users)
3. Images (Logged-in users)
4. Albums (Logged-in users)

## Visualization Features

### Structure Visualization
- View image AVL tree
- View layer tree
- View album list
- View specific layer matrix
- View image and its layer tree
- View client B-tree (admin only)

### Reports
User Reports:
- Top 5 images with most layers
- Leaf layer listing
- Layer tree depth
- Layer listings (preorder, inorder, postorder)

Admin Reports:
- Client information lookup
- Client listing by levels

## Technical Requirements
- Language: Fortran
- OS: Cross-platform
- Graphviz: Required for structure visualization
- No external libraries for data structures

## Additional Notes
- All data structures are custom implementations
- Structures persist during runtime
- Image generation supports multiple methods
- Each layer ID must be unique
- Visualization reports are generated in real-time

## Restrictions
- No use of predefined data structure libraries
- All structure visualizations must be generated using Graphviz
- All reports must be viewable within the application interface

## System Architecture
```
Pixel Print Studio
├── User Management (B-Tree)
├── Layer System
│   ├── Sparse Matrix (Pixel Data)
│   └── Binary Search Tree (Layer Storage)
├── Image Management (AVL Tree)
└── Album Organization (Doubly Linked Lists)
```
