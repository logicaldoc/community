package com.logicaldoc.core.dashlet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;

import org.junit.Before;
import org.junit.Test;
import org.springframework.security.core.context.SecurityContextHolder;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.security.Client;
import com.logicaldoc.core.security.Device;
import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.core.security.spring.LDAuthenticationToken;
import com.logicaldoc.core.security.spring.LDSecurityContextRepository;
import com.logicaldoc.core.security.user.User;
import com.logicaldoc.core.security.user.UserDAO;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.plugin.PluginException;
import com.logicaldoc.util.servlet.MockServletRequest;
import com.logicaldoc.util.servlet.MockServletResponse;
import com.logicaldoc.util.servlet.MockServletSession;

/**
 * Test case for {@link DashletContent}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.0.1
 */
public class DashletContentTest extends AbstractCoreTestCase {

	// Instance under test
	private DashletContent testSubject = new DashletContent();

	protected Session session;

	protected MockServletSession servletSession = new MockServletSession();

	protected final File responseFile = new File("target/documents.xml");

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();
		FileUtil.delete(responseFile);
		prepareSession("admin", "admin");
	}

	@Override
	public void tearDown() throws SQLException {
		super.tearDown();
		FileUtil.delete(responseFile);
	}

	@Test
	public void testService() throws IOException {
		MockServletRequest mockRequest = new MockServletRequest(servletSession);
		mockRequest.setParameter("locale", "en");
		mockRequest.setParameter("dashletId", "21");

		MockServletResponse response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertTrue(FileUtil.readFile(responseFile).contains("<id>1</id>"));

		FileUtil.delete(responseFile);
		mockRequest.setParameter("dashletId", "22");
		response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertTrue(FileUtil.readFile(responseFile).contains("<id>1</id>"));

		FileUtil.delete(responseFile);
		mockRequest.setParameter("dashletId", "23");
		response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		System.out.println(FileUtil.readFile(responseFile));
		assertEquals("sample content", FileUtil.readFile(responseFile).trim());

		FileUtil.delete(responseFile);
		mockRequest.setParameter("dashletId", "6");
		response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		assertTrue(FileUtil.readFile(responseFile).contains("<![CDATA[message for note 1]]>"));

		FileUtil.delete(responseFile);
		mockRequest.setParameter("dashletId", "9");
		response = new MockServletResponse(responseFile);
		testSubject.service(mockRequest, response);
		response.flushBuffer();
		System.out.println(FileUtil.readFile(responseFile));
		assertEquals("", FileUtil.readFile(responseFile));

	}

	protected void prepareSession(String username, String password) throws PersistenceException {
		UserDAO userDao = Context.get().getBean(UserDAO.class);

		Client client = new Client("xyz", "192.168.2.231", "ghost");
		Device device = new Device();
		device.setBrowser("Firefox");
		device.setBrowserVersion("18");
		device.setOperativeSystem("Windows");
		client.setDevice(device);
		session = SessionManager.get().newSession(username, password, null, client);
		if (session != null) {
			User user = userDao.findByUsernameIgnoreCase(username);
			userDao.initialize(user);
			LDAuthenticationToken token = new LDAuthenticationToken(username);
			token.setSid(session.getSid());
			SecurityContextHolder.getContext().setAuthentication(token);
			LDSecurityContextRepository.bindServletSession(session.getSid(), servletSession);
		}
	}
}