package com.logicaldoc.core;

import java.math.BigDecimal;
import java.sql.Date;
import java.sql.Time;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Map;

import org.springframework.jdbc.InvalidResultSetAccessException;
import org.springframework.jdbc.support.rowset.SqlRowSet;
import org.springframework.jdbc.support.rowset.SqlRowSetMetaData;

/**
 * Wrapper for the default SqlRowSet able to hanle modern Jdbc drivers
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.2
 *
 */
public class SqlRowSetWrapper implements SqlRowSet {

	private static final long serialVersionUID = 1L;

	// The wrapped instance
	private SqlRowSet rowSet;

	public SqlRowSetWrapper(SqlRowSet rowSet) {
		super();
		this.rowSet = rowSet;
	}

	@SuppressWarnings("unused")
	private SqlRowSetWrapper() {
	}

	public boolean absolute(int row) throws InvalidResultSetAccessException {
		return rowSet.absolute(row);
	}

	public void afterLast() throws InvalidResultSetAccessException {
		rowSet.afterLast();
	}

	public void beforeFirst() throws InvalidResultSetAccessException {
		rowSet.beforeFirst();
	}

	public int findColumn(String columnLabel) throws InvalidResultSetAccessException {
		return rowSet.findColumn(columnLabel);
	}

	public boolean first() throws InvalidResultSetAccessException {
		return rowSet.first();
	}

	public BigDecimal getBigDecimal(int arg0) throws InvalidResultSetAccessException {
		return rowSet.getBigDecimal(arg0);
	}

	public BigDecimal getBigDecimal(String arg0) throws InvalidResultSetAccessException {
		return rowSet.getBigDecimal(arg0);
	}

	public boolean getBoolean(int columnIndex) throws InvalidResultSetAccessException {
		return rowSet.getBoolean(columnIndex);
	}

	public boolean getBoolean(String columnLabel) throws InvalidResultSetAccessException {
		return rowSet.getBoolean(columnLabel);
	}

	public byte getByte(int columnIndex) throws InvalidResultSetAccessException {
		return rowSet.getByte(columnIndex);
	}

	public byte getByte(String columnLabel) throws InvalidResultSetAccessException {
		return rowSet.getByte(columnLabel);
	}

	public Date getDate(int arg0, Calendar arg1) throws InvalidResultSetAccessException {
		return rowSet.getDate(arg0, arg1);
	}

	public Date getDate(int arg0) throws InvalidResultSetAccessException {
		try {
			return rowSet.getDate(arg0);
		} catch (ClassCastException ce) {
			return getDate(rowSet.getObject(arg0));
		}
	}

	public Date getDate(String arg0, Calendar arg1) throws InvalidResultSetAccessException {
		return rowSet.getDate(arg0, arg1);
	}

	public Date getDate(String arg0) throws InvalidResultSetAccessException {
		try {
			return rowSet.getDate(arg0);
		} catch (ClassCastException ce) {
			return getDate(rowSet.getObject(arg0));
		}
	}

	private Date getDate(Object obj) {
		if (obj instanceof Date)
			return (Date) obj;
		else if (obj instanceof Timestamp)
			return new Date(java.util.Date
					.from(((Timestamp) obj).toLocalDateTime().atZone(ZoneId.systemDefault()).toInstant()).getTime());
		else if (obj instanceof java.util.Date)
			return new Date(((java.util.Date) obj).getTime());
		else if (obj instanceof LocalDateTime)
			return new Date(
					java.util.Date.from(((LocalDateTime) obj).atZone(ZoneId.systemDefault()).toInstant()).getTime());
		return null;
	}

	public double getDouble(int columnIndex) throws InvalidResultSetAccessException {
		return rowSet.getDouble(columnIndex);
	}

	public double getDouble(String columnLabel) throws InvalidResultSetAccessException {
		return rowSet.getDouble(columnLabel);
	}

	public float getFloat(int columnIndex) throws InvalidResultSetAccessException {
		return rowSet.getFloat(columnIndex);
	}

	public float getFloat(String columnLabel) throws InvalidResultSetAccessException {
		return rowSet.getFloat(columnLabel);
	}

	public int getInt(int columnIndex) throws InvalidResultSetAccessException {
		return rowSet.getInt(columnIndex);
	}

	public int getInt(String columnLabel) throws InvalidResultSetAccessException {
		return rowSet.getInt(columnLabel);
	}

	public long getLong(int columnIndex) throws InvalidResultSetAccessException {
		return rowSet.getLong(columnIndex);
	}

	public long getLong(String columnLabel) throws InvalidResultSetAccessException {
		return rowSet.getLong(columnLabel);
	}

	public SqlRowSetMetaData getMetaData() {
		return rowSet.getMetaData();
	}

	public String getNString(int arg0) throws InvalidResultSetAccessException {
		return rowSet.getNString(arg0);
	}

	public String getNString(String arg0) throws InvalidResultSetAccessException {
		return rowSet.getNString(arg0);
	}

	public <T> T getObject(int arg0, Class<T> arg1) throws InvalidResultSetAccessException {
		return rowSet.getObject(arg0, arg1);
	}

	public Object getObject(int arg0, Map<String, Class<?>> arg1) throws InvalidResultSetAccessException {
		return rowSet.getObject(arg0, arg1);
	}

	public Object getObject(int arg0) throws InvalidResultSetAccessException {
		return rowSet.getObject(arg0);
	}

	public <T> T getObject(String arg0, Class<T> arg1) throws InvalidResultSetAccessException {
		return rowSet.getObject(arg0, arg1);
	}

	public Object getObject(String arg0, Map<String, Class<?>> arg1) throws InvalidResultSetAccessException {
		return rowSet.getObject(arg0, arg1);
	}

	public Object getObject(String arg0) throws InvalidResultSetAccessException {
		return rowSet.getObject(arg0);
	}

	public int getRow() throws InvalidResultSetAccessException {
		return rowSet.getRow();
	}

	public short getShort(int columnIndex) throws InvalidResultSetAccessException {
		return rowSet.getShort(columnIndex);
	}

	public short getShort(String columnLabel) throws InvalidResultSetAccessException {
		return rowSet.getShort(columnLabel);
	}

	public String getString(int arg0) throws InvalidResultSetAccessException {
		return rowSet.getString(arg0);
	}

	public String getString(String arg0) throws InvalidResultSetAccessException {
		return rowSet.getString(arg0);
	}

	public Time getTime(int arg0, Calendar arg1) throws InvalidResultSetAccessException {
		return rowSet.getTime(arg0, arg1);
	}

	public Time getTime(int arg0) throws InvalidResultSetAccessException {
		return rowSet.getTime(arg0);
	}

	public Time getTime(String arg0, Calendar arg1) throws InvalidResultSetAccessException {
		return rowSet.getTime(arg0, arg1);
	}

	public Time getTime(String arg0) throws InvalidResultSetAccessException {
		return rowSet.getTime(arg0);
	}

	public Timestamp getTimestamp(int arg0, Calendar arg1) throws InvalidResultSetAccessException {
		return rowSet.getTimestamp(arg0, arg1);
	}

	public Timestamp getTimestamp(int arg0) throws InvalidResultSetAccessException {
		return rowSet.getTimestamp(arg0);
	}

	public Timestamp getTimestamp(String arg0, Calendar arg1) throws InvalidResultSetAccessException {
		return rowSet.getTimestamp(arg0, arg1);
	}

	public Timestamp getTimestamp(String arg0) throws InvalidResultSetAccessException {
		return rowSet.getTimestamp(arg0);
	}

	public boolean isAfterLast() throws InvalidResultSetAccessException {
		return rowSet.isAfterLast();
	}

	public boolean isBeforeFirst() throws InvalidResultSetAccessException {
		return rowSet.isBeforeFirst();
	}

	public boolean isFirst() throws InvalidResultSetAccessException {
		return rowSet.isFirst();
	}

	public boolean isLast() throws InvalidResultSetAccessException {
		return rowSet.isLast();
	}

	public boolean last() throws InvalidResultSetAccessException {
		return rowSet.last();
	}

	public boolean next() throws InvalidResultSetAccessException {
		return rowSet.next();
	}

	public boolean previous() throws InvalidResultSetAccessException {
		return rowSet.previous();
	}

	public boolean relative(int rows) throws InvalidResultSetAccessException {
		return rowSet.relative(rows);
	}

	public boolean wasNull() throws InvalidResultSetAccessException {
		return rowSet.wasNull();
	}

}
