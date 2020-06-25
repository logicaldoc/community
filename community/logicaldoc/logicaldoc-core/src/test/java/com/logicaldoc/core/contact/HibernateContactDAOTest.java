package com.logicaldoc.core.contact;

import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateContactDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8
 */
public class HibernateContactDAOTest extends AbstractCoreTCase {

	// Instance under test
	private ContactDAO dao;

	@Before
	public void setUp() throws Exception {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateContactDAO
		dao = (ContactDAO) context.getBean("ContactDAO");
	}

	@Test
	public void testFindByUser() {
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