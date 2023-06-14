package com.logicaldoc.webservice;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;

/**
 * Test case for {@link HibernateWebserviceCallDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class HibernateWebserviceCallDAOTest extends AbstractWebserviceTestCase {

	private WebserviceCallDAO testSubject;

	@Before
	public void setUp() throws Exception {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an WebserviceCallDAO
		testSubject = (WebserviceCallDAO) context.getBean(WebserviceCallDAO.class);
	}

	@Test
	public void testStore() throws PersistenceException {
		WebserviceCall call = new WebserviceCall();
		call.setTenantId(1L);
		call.setUserId(1L);
		testSubject.store(call);

		List<WebserviceCall> calls = testSubject.findAll(1L);
		assertEquals(1, calls.size());
	}
}