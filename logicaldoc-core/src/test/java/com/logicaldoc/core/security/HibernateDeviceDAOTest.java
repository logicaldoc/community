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
import com.logicaldoc.util.spring.Context;

/**
 * Test case for <code>HibernateUserHistoryDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.0
 */
public class HibernateDeviceDAOTest extends AbstractCoreTestCase {

	// Instance under test
	private DeviceDAO dao;

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDeviceDAO
		dao = Context.get(DeviceDAO.class);
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

		dao.store(device);
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
		dao.store(device);

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
		dao.store(device);

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
		dao.store(device);

		assertEquals(3, dao.findAll(1L).size());
		assertEquals(1, dao.findTrustedDevices(1L).size());
		assertEquals(0, dao.findTrustedDevices(3L).size());
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
		dao.store(device);

		device = new Device();
		device.setCreation(DateBean.dateFromCompactString("20061201"));
		device.setLastLogin(DateBean.dateFromCompactString("20061204"));
		device.setUsername("admin");
		device.setUserId(3L);
		device.setTenantId(1L);
		device.setBrowser("Firefox");
		device.setOperativeSystem("Windows 10");
		device.setType("COMPUTER");
		dao.store(device);

		assertEquals(2, dao.findAll(1L).size());

		dao.cleanOldDevices(1);
		List<Device> devices = dao.findAll();
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
		dao.store(device);

		String id = device.getDeviceId();
		Device requestDevice = new Device();
		requestDevice.setDeviceId(id);

		device = dao.findByDevice(requestDevice);
		assertNotNull(device);
		assertEquals(id, device.getDeviceId());

		requestDevice = new Device();
		requestDevice.setBrowser("Firefox");
		requestDevice.setOperativeSystem("Windows 10");
		requestDevice.setUserId(3L);
		device = dao.findByDevice(requestDevice);
		assertNull(device);

		requestDevice = new Device();
		requestDevice.setBrowser("Firefox");
		requestDevice.setOperativeSystem("Windows 10");
		requestDevice.setType("COMPUTER");
		requestDevice.setUserId(3L);
		device = dao.findByDevice(requestDevice);
		assertNotNull(device);
		assertEquals(id, device.getDeviceId());
	}
}