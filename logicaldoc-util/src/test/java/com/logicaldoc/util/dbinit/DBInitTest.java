package com.logicaldoc.util.dbinit;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import junit.framework.TestCase;

/**
 * Test case for database initialisation utility
 * 
 * @author Marco Meschieri
 */
public class DBInitTest extends TestCase {

	// Instance under test
	private DBInit testSubject;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		List<String> sqlList = new ArrayList<>();
		sqlList.add("sql1.sql");
		sqlList.add("sql2.sql");
		testSubject = new DBInit(sqlList);
		assertEquals(2, testSubject.getSqlList().size());

		// dbinit.setDriver("org.hsqldb.jdbcDriver"); // version 1.8
		testSubject.setDriver("org.hsqldb.jdbc.JDBCDriver");
		testSubject.setUrl("jdbc:hsqldb:mem:logicaldoc");
		testSubject.setUsername("sa");
		testSubject.setPassword("");
		testSubject.setDbms("hsqldb");

		assertEquals("hsqldb", testSubject.getDbms());
	}

	@Test
	public void testExecute() {
		try {
			assertTrue(testSubject.testConnection());
			testSubject.execute();
			assertTrue(testSubject.isConnected());
			testSubject.executeSql("select count(*) from co_menus;");
			testSubject.rollback();
		} finally {
			// Make sure to leave HSQLDB down
			testSubject.executeSql("shutdown");
		}
	}
}
