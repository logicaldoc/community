package com.logicaldoc.core.security.dao;

import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;
import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.i18n.DateBean;
import com.logicaldoc.core.security.Device;

import junit.framework.Assert;

/**
 * Test case for <code>HibernateUserHistoryDAO</code>
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 5.0
 */
public class HibernateDeviceDAOTest extends AbstractCoreTCase {

	// Instance under test
	private DeviceDAO dao;

	@Before
	public void setUp() throws Exception {
		super.setUp();

		// Retrieve the instance under test from spring context. Make sure that
		// it is an HibernateDeviceDAO
		dao = (DeviceDAO) context.getBean("DeviceDAO");
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

		Assert.assertTrue(dao.store(device));
		Assert.assertNotNull(device.getDeviceId());
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
		Assert.assertTrue(dao.store(device));

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
		Assert.assertTrue(dao.store(device));

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
		Assert.assertTrue(dao.store(device));

		Assert.assertEquals(3, dao.findAll(1L).size());
		Assert.assertEquals(1, dao.findTrustedDevices(1L).size());
		Assert.assertEquals(0, dao.findTrustedDevices(3L).size());
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
		Assert.assertTrue(dao.store(device));

		device = new Device();
		device.setCreation(DateBean.dateFromCompactString("20061201"));
		device.setLastLogin(DateBean.dateFromCompactString("20061204"));
		device.setUsername("admin");
		device.setUserId(3L);
		device.setTenantId(1L);
		device.setBrowser("Firefox");
		device.setOperativeSystem("Windows 10");
		device.setType("COMPUTER");
		Assert.assertTrue(dao.store(device));

		Assert.assertEquals(2, dao.findAll(1L).size());

		dao.cleanOldDevices(1);
		List<Device> devices = dao.findAll();
		Assert.assertEquals(1, devices.size());
		Assert.assertEquals("sebastian", devices.get(0).getUsername());
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
		Assert.assertTrue(dao.store(device));

		String id = device.getDeviceId();
		Device requestDevice = new Device();
		requestDevice.setDeviceId(id);

		device = dao.findByDevice(requestDevice);
		Assert.assertNotNull(device);
		Assert.assertEquals(id, device.getDeviceId());

		requestDevice = new Device();
		requestDevice.setBrowser("Firefox");
		requestDevice.setOperativeSystem("Windows 10");
		requestDevice.setUserId(3L);
		device = dao.findByDevice(requestDevice);
		Assert.assertNull(device);

		requestDevice = new Device();
		requestDevice.setBrowser("Firefox");
		requestDevice.setOperativeSystem("Windows 10");
		requestDevice.setType("COMPUTER");
		requestDevice.setUserId(3L);
		device = dao.findByDevice(requestDevice);
		Assert.assertNotNull(device);
		Assert.assertEquals(id, device.getDeviceId());
	}
}