# Deployment Troubleshooting Guide

## ❌ **Database Connection Issues**

### **Problem**: `no such table: especie_region` / `no such table: ind_meta`

### **Root Cause**: Database path inconsistency or missing database file in deployment

### **✅ Fixes Applied**:

1. **Fixed Database Path Inconsistency**:
   ```r
   # Before (inconsistent):
   temp_con <- get_app_connection("db/sibdata.sqlite")  # With path
   con <- get_app_connection()                          # Without path
   
   # After (consistent):
   temp_con <- get_app_connection("db/sibdata.sqlite")  # With path  
   con <- get_app_connection("db/sibdata.sqlite")       # With path
   ```

2. **Added Database Debugging**:
   - Database path resolution logging
   - File existence and size checking
   - Available tables listing
   - Missing tables detection

### **🔍 Debugging Output to Expect**:

```
🗄️ Database connection info:
- Requested path: db/sibdata.sqlite
- Resolved path: /path/to/sibdata/db/sibdata.sqlite
- File exists: TRUE
- File size: 12345678 bytes

📊 Available database tables: especie_region, ind_meta, indicadores, ...
✅ All required tables found
```

### **🚨 Common Deployment Issues**:

1. **Database File Missing**:
   - Ensure `db/sibdata.sqlite` is included in deployment
   - Check file permissions in deployed environment

2. **Path Resolution Issues**:
   - In deployment, paths might resolve differently
   - Use absolute paths if relative paths fail

3. **Database Schema Mismatch**:
   - Ensure deployed database has same schema as development
   - Check if database was updated but app wasn't

### **💡 Deployment Checklist**:

- ✅ Database file `db/sibdata.sqlite` included in deployment
- ✅ Database file has correct permissions
- ✅ Required tables exist: `especie_region`, `ind_meta`, `indicadores`
- ✅ App has read access to database file
- ✅ All functions properly exported with `@export` tags
- ✅ No `source()` calls in app2.R

### **🔧 Quick Fix for Immediate Deployment**:

If database path issues persist, try using absolute path:
```r
# In app2.R, replace relative path with absolute path
con <- get_app_connection("/full/path/to/sibdata/db/sibdata.sqlite")
```

Or use the package's built-in path resolution:
```r
# Let the function use default path resolution
con <- get_app_connection()  # Uses sibdata:::sys_file_sibdata()
```