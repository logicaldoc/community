package com.logicaldoc.core.filler;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for {@link HibernateFillerDAO}
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.3
 */
public class HibernateFillerDAOTest extends AbstractCoreTestCase {
	// Instance under test
	private FillerDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		testSubject = FillerDAO.get();
	}

	@Test
	public void testFindByName() throws Exception {
		MockFiller filler = new MockFiller();
		filler.setName("myFiller");
		filler.setTenantId(1L);
		filler.setDeleted(0);

		testSubject.store(filler);

		Filler found = testSubject.findByName("myFiller", 1L);

		assertNotNull(found);
		assertEquals("myFiller", found.getName());
	}

	@Test
	public void testDelete() throws Exception {
		MockFiller filler = new MockFiller();
		filler.setName("toDelete");
		filler.setTenantId(1L);

		testSubject.store(filler);
		long id = filler.getId();

		testSubject.delete(id, PersistentObject.DELETED_CODE_DEFAULT);

		assertNull(testSubject.findById(id));
	}

	@Test
	public void testFindByType() throws Exception {
		// two valid fillers for tenant 1
		MockFiller a = new MockFiller();
		a.setName("Alpha");
		a.setTenantId(1L);
		testSubject.store(a);

		MockFiller b = new MockFiller();
		b.setName("Beta");
		b.setTenantId(1L);
		testSubject.store(b);

		// one deleted filler for tenant 1
		MockFiller deleted = new MockFiller();
		deleted.setName("Zeta");
		deleted.setTenantId(1L);
		testSubject.store(deleted);
		testSubject.delete(deleted.getId(), 1);

		// one filler for another tenant
		MockFiller otherTenant = new MockFiller();
		otherTenant.setName("Gamma");
		otherTenant.setTenantId(2L);
		testSubject.store(otherTenant);

		List<MockFiller> result = testSubject.findByType(MockFiller.class, 1L);

		assertEquals(2, result.size());
		assertEquals("Alpha", result.get(0).getName());
		assertEquals("Beta", result.get(1).getName());
	}
}
