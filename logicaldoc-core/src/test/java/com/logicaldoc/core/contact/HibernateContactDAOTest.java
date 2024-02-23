package com.logicaldoc.core.contact;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateContactDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8
 */
public class HibernateContactDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private ContactDAO dao;

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateContactDAO
		dao = (ContactDAO) context.getBean("ContactDAO");
	}

	@Test
	public void testFindByUser() throws PersistenceException {
		List<Contact> contacts = dao.findByUser(null, null);
		Assert.assertEquals(1, contacts.size());
		contacts = dao.findByUser(1L, null);
		Assert.assertEquals(2, contacts.size());
		contacts = dao.findByUser(1L, "alessandro@acme.com");
		Assert.assertEquals(1, contacts.size());
		contacts = dao.findByUser(1L, "xxx");
		Assert.assertEquals(0, contacts.size());
	}
}