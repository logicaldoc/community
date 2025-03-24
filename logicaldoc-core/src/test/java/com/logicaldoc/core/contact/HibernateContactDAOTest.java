package com.logicaldoc.core.contact;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for {@link HibernateContactDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.8
 */
public class HibernateContactDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private ContactDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateContactDAO
		testSubject = Context.get(ContactDAO.class);
	}

	@Test
	public void testFindByUser() throws PersistenceException {
		List<Contact> contacts = testSubject.findByUser(null, null);
		assertEquals(1, contacts.size());
		contacts = testSubject.findByUser(1L, null);
		assertEquals(2, contacts.size());
		contacts = testSubject.findByUser(1L, "alessandro@acme.com");
		assertEquals(1, contacts.size());
		contacts = testSubject.findByUser(1L, "xxx");
		assertEquals(0, contacts.size());

		Contact contact1 = testSubject.findById(1L);
		assertNotNull(contact1);

		Contact contact2 = new Contact(contact1);
		assertNotNull(contact2);
		assertNotSame(null, contact2.getFullName());
		assertNotSame(null, contact2.toString());

		assertNotSame(contact1.hashCode(), contact2.hashCode());

		// test equals()
		assertEquals(true, contact1.equals(contact1));
		assertEquals(false, contact1.equals(contact2));

		Contact contact3 = new Contact();
		contact3.setId(contact1.getId());
		assertEquals(false, contact1.equals(contact3));

		assertEquals(false, contact3.equals(contact1));

		contact1.setEmail("test@email.com");
		contact1.setId(2);
		contact2.setEmail("test@email.com");
		contact2.setId(2);
		contact2.setUserId(-5L);

		assertEquals(false, contact1.equals(contact2));

		contact1.setUserId(-3L);
		assertEquals(false, contact1.equals(contact2));
		
		assertNull(testSubject.findById(4L));
	}

	@Test
	public void testCreateContact() throws PersistenceException {
		Contact contact = new Contact();
		contact.setUserId(1L);
		contact.setFirstName("Gino");
		contact.setLastName("Marzulli");
		contact.setCompany("companyName");
		contact.setEmail("marzulli@mail.com");
		contact.setPhone("211235709");
		contact.setMobile("098234098");
		contact.setAddress("addressName");

		testSubject.store(contact);
		assertNotNull(contact);
		
		Long nextVal = testSubject.queryForLong("select next_val from ld_hilo where sequence_name='ld_contact'");
		assertEquals(100L, nextVal.longValue());
	}

}