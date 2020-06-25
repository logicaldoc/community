package com.logicaldoc.core.stats;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.http.Consts;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.document.dao.DocumentDAO;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.UserEvent;
import com.logicaldoc.core.security.dao.GroupDAO;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.core.util.UserUtil;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.http.HttpUtil;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * Collects statistical informations to the stats site
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class StatsCollector extends Task {
	public static final String STAT = "stat";

	public final static String NAME = "StatsCollector";

	private DocumentDAO documentDAO;

	private FolderDAO folderDAO;

	private GroupDAO groupDAO;

	protected GenericDAO genericDAO;

	protected TenantDAO tenantDAO;

	protected ContextProperties config;

	private static String userno = "community";

	private static String product = "LogicalDOC";

	private static String productName = "LogicalDOC Community";

	public StatsCollector() {
		super(NAME);
		log = LoggerFactory.getLogger(StatsCollector.class);
	}

	@Override
	public long getSize() {
		return 8;
	}

	@Override
	protected void runTask() throws Exception {
		log.info("Start statistics collection");

		/*
		 * Collect identification data
		 */
		String id = config.getProperty("id");
		String release = config.getProperty("product.release");
		log.debug("Collected identification data");

		EMailSender sender = new EMailSender(Tenant.DEFAULT_NAME);
		String email = sender.getSender();
		log.debug("Collected contact data");

		/*
		 * Collect environment data
		 */
		String osname = System.getProperty("os.name");
		String osversion = System.getProperty("os.version");
		String javaversion = System.getProperty("java.version");
		String javavendor = System.getProperty("java.vendor");
		String fileencoding = System.getProperty("file.encoding");
		String userlanguage = System.getProperty("user.language");
		String usercountry = System.getProperty("user.country");
		String javaarch = System.getProperty("sun.arch.data.model");
		String dbms = config.getProperty("jdbc.dbms");

		log.debug("Collected environment data");

		/*
		 * Collect registration data
		 */
		String regName = config.getProperty("reg.name");
		String regEmail = config.getProperty("reg.email");
		String regOrganization = config.getProperty("reg.organization");
		String regWebsite = config.getProperty("reg.website");

		next();
		if (interruptRequested)
			return;

		/*
		 * Collect users data
		 */
		int users = userDao.count(null);
		int guests = userDao.countGuests(null);
		int groups = groupDAO.count();
		log.debug("Collected users data");

		long userdir = 0;
		File userDir = UserUtil.getUsersDir();
		userdir = FileUtils.sizeOfDirectory(userDir);
		saveStatistic("userdir", userdir, Tenant.SYSTEM_ID);

		long indexdir = 0;
		File indexDir = new File(config.getPropertyWithSubstitutions("index.dir"));
		if (indexDir.exists())
			indexdir = FileUtils.sizeOfDirectory(indexDir);
		saveStatistic("indexdir", indexdir, Tenant.SYSTEM_ID);

		long importdir = 0;
		File importDir = new File(config.getPropertyWithSubstitutions("conf.importdir"));
		if (importDir.exists())
			importdir = FileUtils.sizeOfDirectory(importDir);
		saveStatistic("importdir", importdir, Tenant.SYSTEM_ID);

		long exportdir = 0;
		File exportDir = new File(config.getPropertyWithSubstitutions("conf.exportdir"));
		if (exportDir.exists())
			exportdir = FileUtils.sizeOfDirectory(exportDir);
		saveStatistic("exportdir", exportdir, Tenant.SYSTEM_ID);

		long plugindir = 0;
		File pluginsDir = PluginRegistry.getPluginsDir();
		plugindir = FileUtils.sizeOfDirectory(pluginsDir);
		saveStatistic("plugindir", plugindir, Tenant.SYSTEM_ID);

		next();
		if (interruptRequested)
			return;

		long dbdir = 0;

		/*
		 * Try to determine the database size by using proprietary queries
		 */
		try {
			if ("mysql".equals(documentDAO.getDbms()))
				dbdir = documentDAO.queryForLong(
						"select sum(data_length+index_length) from information_schema.tables where table_schema=database();");
			else if (documentDAO.isOracle())
				dbdir = documentDAO.queryForLong("SELECT sum(bytes) FROM user_segments");
			else if ("postgresql".equals(documentDAO.getDbms()))
				dbdir = documentDAO.queryForLong("select pg_database_size(current_database())");
		} catch (Throwable t) {
			log.warn("Unable to determine the database size - {}", t.getMessage());
		}

		/*
		 * Fall back to the database dir
		 */
		if (dbdir == 0) {
			File dbDir = new File(config.getPropertyWithSubstitutions("conf.dbdir"));
			if (dbDir.exists())
				dbdir = FileUtils.sizeOfDirectory(dbDir);
		}
		saveStatistic("dbdir", dbdir, Tenant.SYSTEM_ID);

		long logdir = 0;
		File logsDir = new File(config.getPropertyWithSubstitutions("conf.logdir"));
		if (logsDir.exists())
			logdir = FileUtils.sizeOfDirectory(logsDir);
		saveStatistic("logdir", logdir, Tenant.SYSTEM_ID);

		log.info("Saved repository statistics");
		next();
		if (interruptRequested)
			return;

		/*
		 * Collect documents statistics
		 */
		long[] docStats = extractDocStats(Tenant.SYSTEM_ID);
		long totaldocs = docStats[3];
		long archiveddocs = docStats[4];
		long docdir = docStats[5];
		
		List<Tenant> tenants = tenantDAO.findAll();
		for (Tenant tenant : tenants)
			extractDocStats(tenant.getId());

		log.info("Saved documents statistics");
		next();
		if (interruptRequested)
			return;

		/*
		 * Collect folders statistics
		 */
		long[] fldStats = extractFldStats(Tenant.SYSTEM_ID);
		long withdocs = fldStats[0];
		long empty = fldStats[1];
		long deletedfolders = fldStats[2];

		for (Tenant tenant : tenants)
			extractFldStats(tenant.getId());

		log.info("Saved folder statistics");
		next();
		if (interruptRequested)
			return;

		/*
		 * Collect sizing statistics
		 */
		long tags = folderDAO.queryForLong("SELECT COUNT(*) FROM ld_tag");
		long versions = folderDAO.queryForLong("SELECT COUNT(*) FROM ld_version");
		long histories = folderDAO.queryForLong("SELECT COUNT(*) FROM ld_history");
		long user_histories = folderDAO.queryForLong("SELECT COUNT(*) FROM ld_user_history");
		long votes = folderDAO.queryForLong("SELECT COUNT(*) FROM ld_rating");

		/*
		 * Save the last update time
		 */
		saveStatistic("lastrun", new Date(), Tenant.SYSTEM_ID);

		log.info("Statistics collected");
		next();
		if (interruptRequested)
			return;

		try {
			log.debug("Package collected statistics");

			// Prepare the post parameters
			List<NameValuePair> postParams = new ArrayList<NameValuePair>();

			// Add all statistics as parameters
			postParams.add(new BasicNameValuePair("id", id != null ? id : ""));
			postParams.add(new BasicNameValuePair("userno", userno != null ? userno : ""));
			postParams.add(new BasicNameValuePair("product_release", release != null ? release : ""));
			postParams.add(new BasicNameValuePair("email", email != null ? email : ""));
			postParams.add(
					new BasicNameValuePair("product", StatsCollector.product != null ? StatsCollector.product : ""));
			postParams.add(new BasicNameValuePair("product_name",
					StatsCollector.productName != null ? StatsCollector.productName : ""));

			postParams.add(new BasicNameValuePair("java_version", javaversion != null ? javaversion : ""));
			postParams.add(new BasicNameValuePair("java_vendor", javavendor != null ? javavendor : ""));
			postParams.add(new BasicNameValuePair("java_arch", javaarch != null ? javaarch : ""));
			postParams.add(new BasicNameValuePair("dbms", dbms != null ? dbms : ""));

			postParams.add(new BasicNameValuePair("os_name", osname != null ? osname : ""));
			postParams.add(new BasicNameValuePair("os_version", osversion != null ? osversion : ""));
			postParams.add(new BasicNameValuePair("file_encoding", fileencoding != null ? fileencoding : ""));

			postParams.add(new BasicNameValuePair("user_language", userlanguage != null ? userlanguage : ""));
			postParams.add(new BasicNameValuePair("user_country", usercountry != null ? usercountry : ""));

			// Sizing
			postParams.add(new BasicNameValuePair("users", Integer.toString(users)));
			postParams.add(new BasicNameValuePair("guests", Integer.toString(guests)));
			postParams.add(new BasicNameValuePair("groups", Integer.toString(groups)));
			postParams.add(new BasicNameValuePair("docs", Long.toString(totaldocs)));
			postParams.add(new BasicNameValuePair("archived_docs", Long.toString(archiveddocs)));
			postParams.add(new BasicNameValuePair("folders", Long.toString(withdocs + empty + deletedfolders)));
			postParams.add(new BasicNameValuePair("tags", Long.toString(tags)));
			postParams.add(new BasicNameValuePair("versions", Long.toString(versions)));
			postParams.add(new BasicNameValuePair("histories", Long.toString(histories)));
			postParams.add(new BasicNameValuePair("user_histories", Long.toString(user_histories)));
			postParams.add(new BasicNameValuePair("votes", Long.toString(votes)));

			collectFeatureUsageStats(postParams);
			next();
			if (interruptRequested)
				return;

			/*
			 * General usage
			 */
			SimpleDateFormat isoDf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");

			Date lastLogin = null;
			try {
				lastLogin = (Date) documentDAO
						.queryForObject("select max(ld_date) from ld_user_history where ld_deleted=0 and ld_event='"
								+ UserEvent.LOGIN.toString() + "'", Date.class);
			} catch (Throwable t) {
				log.warn("Unable to retrieve last login statistics - {}", t.getMessage());
			}
			postParams.add(new BasicNameValuePair("last_login", lastLogin != null ? isoDf.format(lastLogin) : ""));

			Date lastCreation = null;
			try {
				lastCreation = (Date) documentDAO
						.queryForObject("select max(ld_date) from ld_history where ld_deleted=0 and ld_event='"
								+ DocumentEvent.STORED + "'", Date.class);
			} catch (Throwable t) {
				log.warn("Unable to retrieve last creation statistics - {}", t.getMessage());
			}
			postParams.add(
					new BasicNameValuePair("last_creation", lastCreation != null ? isoDf.format(lastCreation) : ""));

			/*
			 * Quotas
			 */
			postParams.add(new BasicNameValuePair("docdir", Long.toString(docdir)));
			postParams.add(new BasicNameValuePair("indexdir", Long.toString(indexdir)));
			postParams.add(new BasicNameValuePair("quota",
					Long.toString(docdir + indexdir + userdir + importdir + exportdir + plugindir + dbdir + logdir)));

			/*
			 * Registration
			 */
			postParams.add(new BasicNameValuePair("reg_name", regName != null ? regName : ""));
			postParams.add(new BasicNameValuePair("reg_email", regEmail != null ? regEmail : ""));
			postParams.add(new BasicNameValuePair("reg_organization", regOrganization != null ? regOrganization : ""));
			postParams.add(new BasicNameValuePair("reg_website", regWebsite != null ? regWebsite : ""));

			HttpPost post = new HttpPost("http://stat.logicaldoc.com/stats/collect");
			UrlEncodedFormEntity entity = new UrlEncodedFormEntity(postParams, Consts.UTF_8);
			post.setEntity(entity);

			CloseableHttpClient httpclient = HttpUtil.getNotValidatingClient(60);

			// Execute request
			try (CloseableHttpResponse response = httpclient.execute(post)) {
				int responseStatusCode = response.getStatusLine().getStatusCode();
				// log status code
				log.debug("Response status code: {}", responseStatusCode);
				if (responseStatusCode != 200)
					throw new IOException(HttpUtil.getBodyString(response));
			}

			log.info("Statistics packaged");
			next();
			if (interruptRequested)
				return;
		} catch (Throwable t) {
			log.warn("Troubles packaging the statistics");
			log.debug("Unable to send statistics", t);
		}
	}

	private void collectFeatureUsageStats(List<NameValuePair> postParams) {
		long bookmarks = 0;
		try {
			bookmarks = folderDAO.queryForLong("SELECT COUNT(ld_id) FROM ld_bookmark where ld_deleted=0");
		} catch (Throwable t) {
			log.warn("Unable to calculate bookmarks statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("bookmarks", Long.toString(bookmarks)));

		long links = 0;
		try {
			links = folderDAO.queryForLong("SELECT COUNT(ld_id) FROM ld_link where ld_deleted=0");
		} catch (Throwable t) {
			log.warn("Unable to calculate links statistics - ", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("links", Long.toString(links)));

		long notes = 0;
		try {
			notes = folderDAO.queryForLong("SELECT COUNT(ld_id) FROM ld_note where ld_deleted=0");
		} catch (Throwable t) {
			log.warn("Unable to calculate notes statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("notes", Long.toString(notes)));

		long aliases = 0;
		try {
			aliases = folderDAO.queryForLong("SELECT COUNT(ld_id) FROM ld_document WHERE ld_docref IS NOT NULL");
		} catch (Throwable t) {
			log.warn("Unable to calculate aliases statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("aliases", Long.toString(aliases)));

		long tenants = 0;
		try {
			tenants = folderDAO.queryForLong("SELECT COUNT(ld_id) FROM ld_tenant where ld_deleted=0");
		} catch (Throwable t) {
			log.warn("Unable to calculate tenants statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("tenants", Long.toString(tenants)));

		long workflow_histories = 0;
		try {
			workflow_histories = folderDAO
					.queryForLong("SELECT COUNT(ld_id) FROM ld_workflowhistory where ld_deleted=0");
		} catch (Throwable t) {
			log.warn("Unable to calculate workflow statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("workflow_histories", Long.toString(workflow_histories)));

		long templates = 0;
		try {
			templates = documentDAO.queryForLong("select count(ld_id) from ld_template where ld_deleted=0");
		} catch (Throwable t) {
			log.warn("Unable to calculate templates statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("templates", Long.toString(templates)));

		long importFolders = 0;
		try {
			importFolders = documentDAO.queryForLong("select count(ld_id) from ld_importfolder where ld_deleted=0");
		} catch (Throwable t) {
			log.warn("Unable to calculate import folders statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("importfolders", Long.toString(importFolders)));

		long stamps = 0;
		try {
			stamps = documentDAO.queryForLong("select count(ld_id) from ld_stamp where ld_deleted=0");
		} catch (Throwable t) {
			log.warn("Unable to calculate stamps statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("stamps", Long.toString(stamps)));

		long forms = 0;
		try {
			forms = documentDAO.queryForLong("select count(ld_id) from ld_document where ld_deleted=0 and ld_nature="
					+ AbstractDocument.NATURE_FORM);
		} catch (Throwable t) {
			log.warn("Unable to calculate forms statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("forms", Long.toString(forms)));

		long reports = 0;
		try {
			reports = documentDAO.queryForLong("select count(ld_id) from ld_report where ld_deleted=0");
		} catch (Throwable t) {
			log.warn("Unable to calculate reports statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("reports", Long.toString(reports)));

		// Collect import email account
		long emailAccounts = 0;
		try {
			documentDAO.queryForLong("select count(ld_id) from ld_emailaccount where ld_deleted=0");
		} catch (Throwable t) {
			log.warn("Unable to calculate import email accounts - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("emailaccounts", Long.toString(emailAccounts)));

		// Collect calendar events
		long calendarEvents = 0;
		try {
			documentDAO.queryForLong("select count(ld_id) from ld_event where ld_deleted=0");
		} catch (Throwable t) {
			log.warn("Unable to calculate calendar events - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("calendarevents", Long.toString(calendarEvents)));
	}

	/**
	 * Retrieves the document stats of a specific tenant and saves the results
	 * in the database
	 * 
	 * @param tenantId The tenant Id if the system tenant is used the whole
	 *        stats are computed
	 * @return Ordered list of stats<br/>
	 *         <ol>
	 *         <li>notindexeddocs</li>
	 *         <li>indexeddocs</li>
	 *         <li>deleteddocs</li>
	 *         <li>totaldocs</li>
	 *         <li>archiveddocs</li>
	 *         <li>docdir (total size of the whole repository)</li>
	 *         <li>notindexabledocs</li>
	 *         </ol>
	 */
	private long[] extractDocStats(long tenantId) {
		long[] stats = new long[7];
		stats[0] = 0;
		try {
			stats[0] = documentDAO.queryForLong(
					"SELECT COUNT(A.ld_id) FROM ld_document A where A.ld_indexed = 0 and A.ld_deleted = 0 "
							+ (tenantId != Tenant.SYSTEM_ID ? " and A.ld_tenantid=" + tenantId : "")
							+ " and not A.ld_status=" + AbstractDocument.DOC_ARCHIVED);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[1] = 0;
		try {
			stats[1] = documentDAO.queryForLong(
					"SELECT COUNT(A.ld_id) FROM ld_document A where A.ld_indexed = 1 and A.ld_deleted = 0 "
							+ (tenantId != Tenant.SYSTEM_ID ? " and A.ld_tenantid=" + tenantId : "")
							+ " and not A.ld_status=" + AbstractDocument.DOC_ARCHIVED);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[2] = 0;
		try {
			stats[2] = documentDAO.queryForLong("SELECT COUNT(A.ld_id) FROM ld_document A where A.ld_deleted  > 0 "
					+ (tenantId != Tenant.SYSTEM_ID ? " and A.ld_tenantid=" + tenantId : ""));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		if (tenantId != Tenant.SYSTEM_ID) {
			stats[3] = documentDAO.count(tenantId, true, true);
		} else {
			stats[3] = documentDAO.count(null, true, true);
		}

		stats[4] = 0;
		try {
			stats[4] = documentDAO.queryForLong(
					"SELECT COUNT(A.ld_id) FROM ld_document A where A.ld_status = " + AbstractDocument.DOC_ARCHIVED
							+ (tenantId != Tenant.SYSTEM_ID ? " and A.ld_tenantid=" + tenantId : ""));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[5] = documentDAO.computeTotalSize(tenantId, null, true);

		stats[6] = 0;
		try {
			stats[6] = documentDAO.queryForLong("SELECT COUNT(A.ld_id) FROM ld_document A where A.ld_indexed = "
					+ AbstractDocument.INDEX_SKIP + " and A.ld_deleted = 0 "
					+ (tenantId != Tenant.SYSTEM_ID ? " and A.ld_tenantid=" + tenantId : "") + " and not A.ld_status="
					+ AbstractDocument.DOC_ARCHIVED);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		saveStatistic("notindexeddocs", stats[0], tenantId);
		saveStatistic("indexeddocs", stats[1], tenantId);
		saveStatistic("deleteddocs", stats[2], tenantId);
		saveStatistic("totaldocs", stats[3], tenantId);
		saveStatistic("archiveddocs", stats[4], tenantId);
		saveStatistic("docdir", stats[5], tenantId);
		saveStatistic("notindexabledocs", stats[6], tenantId);

		return stats;
	}

	/**
	 * Retrieves the folder stats of a specific tenant and saves the results in
	 * the database
	 * 
	 * @param tenantId The tenant Id if the system tenant is used the whole
	 *        stats are computed
	 * @return Ordered list of stats<br/>
	 *         <ol>
	 *         <li>withdocs</li>
	 *         <li>empty</li>
	 *         <li>deletedfolders</li>
	 *         </ol>
	 */
	private long[] extractFldStats(long tenantId) {
		long[] stats = new long[4];

		stats[0] = 0;
		try {
			stats[0] = folderDAO.queryForLong(
					"SELECT COUNT(A.ld_id) FROM ld_folder A where A.ld_deleted = 0 and A.ld_id in (select B.ld_folderid FROM ld_document B where B.ld_deleted = 0) "
							+ (tenantId != Tenant.SYSTEM_ID ? " and A.ld_tenantid=" + tenantId : ""));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[1] = 0;
		try {
			stats[1] = folderDAO.queryForLong(
					"SELECT COUNT(A.ld_id) FROM ld_folder A where A.ld_deleted = 0 and A.ld_id not in (select B.ld_folderid FROM ld_document B where B.ld_deleted = 0) "
							+ (tenantId != Tenant.SYSTEM_ID ? " and A.ld_tenantid=" + tenantId : ""));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[2] = 0;
		try {
			stats[2] = folderDAO.queryForLong("SELECT COUNT(A.ld_id) FROM ld_folder A where A.ld_deleted  > 0 "
					+ (tenantId != Tenant.SYSTEM_ID ? " and A.ld_tenantid=" + tenantId : ""));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		saveStatistic("withdocs", stats[0], tenantId);
		saveStatistic("empty", stats[1], tenantId);
		saveStatistic("deletedfolders", stats[2], tenantId);

		return stats;
	}

	/**
	 * Convenience method for saving statistical data in the DB as Generics
	 */
	protected void saveStatistic(String parameter, Object val, long tenantId) {
		Generic gen = genericDAO.findByAlternateKey(STAT, parameter, null, tenantId);
		if (gen == null) {
			gen = new Generic();
			gen.setType(STAT);
			gen.setTenantId(tenantId);
			gen.setSubtype(parameter);
		} else
			genericDAO.initialize(gen);

		if (val instanceof Date)
			gen.setDate1((Date) val);
		else if (val instanceof String)
			gen.setString1((String) val);
		else if (val instanceof Long)
			gen.setInteger1((Long) val);
		else
			gen.setInteger1(((Integer) val).longValue());

		try {
			genericDAO.store(gen);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}
	}

	@Override
	public boolean isIndeterminate() {
		return false;
	}

	@Override
	public boolean isConcurrent() {
		return true;
	}

	public GenericDAO getGenericDAO() {
		return genericDAO;
	}

	public void setGenericDAO(GenericDAO genericDAO) {
		this.genericDAO = genericDAO;
	}

	public ContextProperties getConfig() {
		return config;
	}

	public void setConfig(ContextProperties config) {
		this.config = config;
	}

	public DocumentDAO getDocumentDAO() {
		return documentDAO;
	}

	public void setDocumentDAO(DocumentDAO documentDAO) {
		this.documentDAO = documentDAO;
	}

	public FolderDAO getFolderDAO() {
		return folderDAO;
	}

	public void setFolderDAO(FolderDAO folderDAO) {
		this.folderDAO = folderDAO;
	}

	public GroupDAO getGroupDAO() {
		return groupDAO;
	}

	public void setGroupDAO(GroupDAO groupDAO) {
		this.groupDAO = groupDAO;
	}

	public static void setUserno(String userno) {
		StatsCollector.userno = userno;
	}

	public static void setProduct(String product) {
		StatsCollector.product = product;
	}

	public static void setProductName(String productName) {
		StatsCollector.productName = productName;
	}

	public void setTenantDAO(TenantDAO tenantDAO) {
		this.tenantDAO = tenantDAO;
	}
}