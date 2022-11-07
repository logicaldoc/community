package com.logicaldoc.webservice;

import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.PersistenceException;

import junit.framework.Assert;


/**
 * Test case for {@link HibernateWebserviceCallDAO}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7
 */
public class HibernateWebserviceCallDAOTest extends AbstractWebserviceTCase {

	// Instance under test
	private WebserviceCallDAO dao;

	@Before
	public void setUp() throws Exception {
		super.setUp();
		// Retrieve the instance under test from spring context. Make sure that
		// it is an WebserviceCallDAO
		dao = (WebserviceCallDAO) context.getBean(WebserviceCallDAO.class);
	}
	
	@Test
	public void testStore() throws PersistenceException {
		WebserviceCall call=new WebserviceCall();
		call.setTenantId(1L);
		call.setUserId(1L);
		dao.store(call);
		
		List<WebserviceCall> calls = dao.findAll(1L);
		Assert.assertEquals(1, calls.size());
	}
}