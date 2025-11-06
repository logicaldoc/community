package com.logicaldoc.core.security;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.i18n.DateBean;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Test case for <code>HibernateUserHistoryDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.0
 */
public class HibernateDeviceDAOTest extends AbstractCoreTestCase {

	private DeviceDAO testSubject;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		testSubject = DeviceDAO.get();
	}

	@Test
	public void testStore() throws PersistenceException {
		Device device = new Device();
		device.setCreation(DateBean.dateFromCompactString("20101201"));
		device.setLastLogin(DateBean.dateFromCompactString("20101204"));
		device.setUsername("sebastian");
		device.setUserId(3L);
		device.setTenantId(1L);
		device.setBrowser("Firefox");
		device.setOperativeSystem("Windows 10");
		device.setType("COMPUTER");

		testSubject.store(device);
		assertNotNull(device.getDeviceId());
	}

	@Test
	public void testFindTrusted() throws PersistenceException {
		Device device = new Device();
		device.setCreation(DateBean.dateFromCompactString("20101201"));
		device.setLastLogin(DateBean.dateFromCompactString("20101204"));
		device.setUsername("sebastian");
		device.setUserId(3L);
		device.setTenantId(1L);
		device.setBrowser("Firefox");
		device.setOperativeSystem("Windows 10");
		device.setType("COMPUTER");
		testSubject.store(device);

		device = new Device();
		device.setCreation(DateBean.dateFromCompactString("20101201"));
		device.setLastLogin(DateBean.dateFromCompactString("20101204"));
		device.setUsername("sebastian");
		device.setUserId(3L);
		device.setTenantId(1L);
		device.setTrusted(0);
		device.setBrowser("Firefox");
		device.setOperativeSystem("Windows 8");
		device.setType("COMPUTER");
		testSubject.store(device);

		device = new Device();
		device.setCreation(DateBean.dateFromCompactString("20101201"));
		device.setLastLogin(DateBean.dateFromCompactString("20101204"));
		device.setUsername("sebastian");
		device.setUserId(1L);
		device.setTenantId(1L);
		device.setTrusted(1);
		device.setBrowser("Firefox");
		device.setOperativeSystem("Windows 7");
		device.setType("COMPUTER");
		testSubject.store(device);

		assertEquals(3, testSubject.findAll(1L).size());
		assertEquals(1, testSubject.findTrustedDevices(1L).size());
		assertEquals(0, testSubject.findTrustedDevices(3L).size());
	}

	@Test
	public void testCleanOldDevices() throws PersistenceException {
		Device device = new Device();
		device.setLastLogin(new Date());
		device.setUsername("sebastian");
		device.setUserId(3L);
		device.setTenantId(1L);
		device.setBrowser("Firefox");
		device.setOperativeSystem("Windows 10");
		device.setType("COMPUTER");
		testSubject.store(device);

		device = new Device();
		device.setCreation(DateBean.dateFromCompactString("20061201"));
		device.setLastLogin(DateBean.dateFromCompactString("20061204"));
		device.setUsername("admin");
		device.setUserId(3L);
		device.setTenantId(1L);
		device.setBrowser("Firefox");
		device.setOperativeSystem("Windows 10");
		device.setType("COMPUTER");
		testSubject.store(device);

		assertEquals(2, testSubject.findAll(1L).size());

		testSubject.cleanOldDevices(1);
		List<Device> devices = testSubject.findAll();
		assertEquals(1, devices.size());
		assertEquals("sebastian", devices.get(0).getUsername());
	}

	@Test
	public void testFindByDevice() throws PersistenceException {
		Device device = new Device();
		device.setCreation(DateBean.dateFromCompactString("20101201"));
		device.setLastLogin(DateBean.dateFromCompactString("20101204"));
		device.setUsername("sebastian");
		device.setUserId(3L);
		device.setTenantId(1L);
		device.setBrowser("Firefox");
		device.setOperativeSystem("Windows 10");
		device.setType("COMPUTER");
		testSubject.store(device);

		String id = device.getDeviceId();
		Device requestDevice = new Device();
		requestDevice.setDeviceId(id);

		device = testSubject.findByDevice(requestDevice);
		assertNotNull(device);
		assertEquals(id, device.getDeviceId());

		requestDevice = new Device();
		requestDevice.setBrowser("Firefox");
		requestDevice.setOperativeSystem("Windows 10");
		requestDevice.setUserId(3L);
		device = testSubject.findByDevice(requestDevice);
		assertNull(device);

		requestDevice = new Device();
		requestDevice.setBrowser("Firefox");
		requestDevice.setOperativeSystem("Windows 10");
		requestDevice.setType("COMPUTER");
		requestDevice.setUserId(3L);
		device = testSubject.findByDevice(requestDevice);
		assertNotNull(device);
		assertEquals(id, device.getDeviceId());
	}
}