package com.logicaldoc.webservice.soap.endpoint;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.concurrent.TimeUnit;

import javax.servlet.http.HttpServletRequest;
import javax.xml.ws.WebServiceContext;
import javax.xml.ws.handler.MessageContext;

import org.apache.cxf.transport.http.AbstractHTTPDestination;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Answers;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import com.logicaldoc.util.Context;
import com.logicaldoc.webservice.AbstractWebserviceTestCase;

@RunWith(MockitoJUnitRunner.class)
public class SoapAuthServiceTest extends AbstractWebserviceTestCase {

	@Mock WebServiceContext wscontext;
    @Mock MessageContext messageContext;
    @Mock HttpServletRequest httpRequest;

	// Instance under test
	private SoapAuthService soapAuthServiceImpl;

	@Before
	public void setUp() throws Exception {
		super.setUp();

		wscontext = mock(WebServiceContext.class, Answers.RETURNS_DEEP_STUBS);
		messageContext = mock(MessageContext.class);

		when(wscontext.getMessageContext()).thenReturn(messageContext);
		when(messageContext.get(AbstractHTTPDestination.HTTP_REQUEST)).thenReturn(httpRequest);

		// Make sure that this is a SoapAuthService instance
		soapAuthServiceImpl = new SoapAuthService();
		//soapAuthServiceImpl.setValidateSession(false);
		soapAuthServiceImpl.setContext(wscontext);
	}

	@Test
	public void testLogin() {
		try {
			String sid = soapAuthServiceImpl.login("author", "admin");
			System.err.println(sid);
			assertNotNull(sid);
		} catch (Exception e) {
			e.printStackTrace();
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testLogout() {
		try {
			String sid = soapAuthServiceImpl.login("author", "admin");
			System.err.println(sid);
			assertNotNull(sid);
			soapAuthServiceImpl.logout(sid);
		} catch (Exception e) {
			e.printStackTrace();
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testValid() {
		try {
			String sid = soapAuthServiceImpl.login("author", "admin");
			System.err.println(sid);
			assertNotNull(sid);
			boolean isValid = soapAuthServiceImpl.valid(sid);
			assertTrue(isValid);
			
			Context.get().getProperties().setProperty("webservice.enabled", "false");
			isValid = soapAuthServiceImpl.valid(sid);
			assertFalse(isValid);			
			
			Context.get().getProperties().setProperty("webservice.enabled", "true");
			
			// Now using an invalid sid
			sid = "106a106d-a440-43b9-a3fd-4acb85543a0e";
			isValid = soapAuthServiceImpl.valid(sid);
			assertFalse(isValid);
					
		} catch (Exception e) {
			e.printStackTrace();
			fail("Unexpected exception was thrown");
		}
	}

	@Test
	public void testRenew() {
		try {
			String sid = soapAuthServiceImpl.login("author", "admin");
			System.err.println(sid);
			assertNotNull(sid);
			TimeUnit.SECONDS.sleep(3);
			soapAuthServiceImpl.renew(sid);
		} catch (Exception e) {
			e.printStackTrace();
			fail("Unexpected exception was thrown");
		}
	}

}
