package com.logicaldoc.core.stats;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.annotation.Resource;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.entity.UrlEncodedFormEntity;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.core5.http.NameValuePair;
import org.apache.hc.core5.http.message.BasicNameValuePair;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.core.document.AbstractDocument;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentEvent;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.generic.Generic;
import com.logicaldoc.core.generic.GenericDAO;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.core.security.user.GroupDAO;
import com.logicaldoc.core.security.user.UserEvent;
import com.logicaldoc.core.sequence.SequenceDAO;
import com.logicaldoc.core.task.Task;
import com.logicaldoc.core.task.TaskException;
import com.logicaldoc.core.util.UserUtil;
import com.logicaldoc.util.http.HttpUtil;
import com.logicaldoc.util.http.StringHttpClientResponseHandler;
import com.logicaldoc.util.plugin.PluginRegistry;

/**
 * Collects statistical informations to the stats site
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class StatsCollector extends Task {
	private static final String AND_NOT_A_LD_STATUS = " and not A.ld_status=";

	private static final String AND_A_LD_TENANTID = " and A.ld_tenantid=";

	public static final String STAT = "stat";

	public static final String NAME = "StatsCollector";

	@Resource(name = "DocumentDAO")
	private DocumentDAO documentDAO;

	@Resource(name = "FolderDAO")
	private FolderDAO folderDAO;

	@Resource(name = "GroupDAO")
	private GroupDAO groupDAO;

	@Resource(name = "GenericDAO")
	protected GenericDAO genericDAO;

	@Resource(name = "TenantDAO")
	protected TenantDAO tenantDAO;

	@Resource(name = "SequenceDAO")
	private SequenceDAO sequenceDAO;

	private static String userno = "community";

	private static String sid;

	private static String product = "LogicalDOC";

	private static String productName = "LogicalDOC Community";

	private boolean uploadStatistics = true;

	private List<NameValuePair> statistics = new ArrayList<>();

	public StatsCollector() {
		super(NAME);
		log = LoggerFactory.getLogger(StatsCollector.class);
	}

	@Override
	public long getSize() {
		return 9;
	}

	@Override
	protected void runTask() throws TaskException {
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

		try {
			/*
			 * Collect users statistics
			 */
			int users = userDao.count(null);
			int guests = userDao.countGuests(null);
			int groups = groupDAO.count();
			log.debug("Collected users data");

			long userdir = calculateUserDirSize();
			saveStatistic("userdir", userdir, Tenant.SYSTEM_ID);

			long indexdir = calculateIndexDirSize();
			saveStatistic("indexdir", indexdir, Tenant.SYSTEM_ID);

			long importdir = calculateImportDirSize();
			saveStatistic("importdir", importdir, Tenant.SYSTEM_ID);

			long exportdir = calculateExportDirSize();
			saveStatistic("exportdir", exportdir, Tenant.SYSTEM_ID);

			long plugindir = calculatePluginDirSize();
			saveStatistic("plugindir", plugindir, Tenant.SYSTEM_ID);

			next();
			if (interruptRequested)
				return;

			long dbdir = calculateDatabaseSize();
			saveStatistic("dbdir", dbdir, Tenant.SYSTEM_ID);

			long logdir = calculateLogDirSize();
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
			long trash = docStats[7];

			calculateAllTenantsDocsStats();

			log.info("Saved documents statistics");
			next();
			if (interruptRequested)
				return;

			/*
			 * Collect pages statistics
			 */

			long[] pageStats = extractPageStats(Tenant.SYSTEM_ID);
			long totalpages = pageStats[3];

			calculateAllTenantsPageStats();

			log.info("Saved pages statistics");
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

			calculateAllTenantsFolderStats();

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
			long userHistories = folderDAO.queryForLong("SELECT COUNT(*) FROM ld_user_history");
			long votes = folderDAO.queryForLong("SELECT COUNT(*) FROM ld_rating");
			long wsCalls = sequenceDAO.getCurrentValue("wscall", 0, Tenant.SYSTEM_ID);

			/*
			 * Save the last update time
			 */
			saveStatistic("lastrun", new Date(), Tenant.SYSTEM_ID);

			log.info("Statistics collected");
			next();
			if (interruptRequested)
				return;

			log.debug("Package collected statistics");

			// Prepare the post parameters
			statistics.clear();

			// Add all statistics as parameters
			statistics.add(new BasicNameValuePair("id", StringUtils.defaultString(id)));
			statistics.add(new BasicNameValuePair("userno", StringUtils.defaultString(userno)));
			statistics.add(new BasicNameValuePair("sid", StringUtils.defaultString(sid)));

			statistics.add(new BasicNameValuePair("product_release", StringUtils.defaultString(release)));
			statistics.add(new BasicNameValuePair("email", StringUtils.defaultString(email)));
			statistics.add(new BasicNameValuePair("product", StringUtils.defaultString(StatsCollector.product)));
			statistics
					.add(new BasicNameValuePair("product_name", StringUtils.defaultString(StatsCollector.productName)));

			statistics.add(new BasicNameValuePair("java_version", StringUtils.defaultString(javaversion)));
			statistics.add(new BasicNameValuePair("java_vendor", StringUtils.defaultString(javavendor)));
			statistics.add(new BasicNameValuePair("java_arch", StringUtils.defaultString(javaarch)));
			statistics.add(new BasicNameValuePair("dbms", StringUtils.defaultString(dbms)));

			statistics.add(new BasicNameValuePair("os_name", StringUtils.defaultString(osname)));
			statistics.add(new BasicNameValuePair("os_version", StringUtils.defaultString(osversion)));
			statistics.add(new BasicNameValuePair("file_encoding", StringUtils.defaultString(fileencoding)));

			statistics.add(new BasicNameValuePair("user_language", StringUtils.defaultString(userlanguage)));
			statistics.add(new BasicNameValuePair("user_country", StringUtils.defaultString(usercountry)));

			// Sizing
			statistics.add(new BasicNameValuePair("users", Integer.toString(users)));
			statistics.add(new BasicNameValuePair("guests", Integer.toString(guests)));
			statistics.add(new BasicNameValuePair("groups", Integer.toString(groups)));
			statistics.add(new BasicNameValuePair("docs", Long.toString(totaldocs)));
			statistics.add(new BasicNameValuePair("pages", Long.toString(totalpages)));

			statistics.add(new BasicNameValuePair("archived_docs", Long.toString(archiveddocs)));
			statistics.add(new BasicNameValuePair("folders", Long.toString(withdocs + empty + deletedfolders)));
			statistics.add(new BasicNameValuePair("tags", Long.toString(tags)));
			statistics.add(new BasicNameValuePair("versions", Long.toString(versions)));
			statistics.add(new BasicNameValuePair("histories", Long.toString(histories)));
			statistics.add(new BasicNameValuePair("user_histories", Long.toString(userHistories)));
			statistics.add(new BasicNameValuePair("votes", Long.toString(votes)));
			statistics.add(new BasicNameValuePair("wscalls", Long.toString(wsCalls)));

			collectFeatureUsageStats(statistics);
			next();
			if (interruptRequested)
				return;

			/*
			 * General usage
			 */
			Date lastLogin = findLastLogin();
			statistics.add(new BasicNameValuePair("last_login", formatDate(lastLogin)));

			Date lastCreation = findLastCreation();
			statistics.add(new BasicNameValuePair("last_creation", formatDate(lastCreation)));

			/*
			 * Quotas
			 */
			statistics.add(new BasicNameValuePair("docdir", Long.toString(docdir)));
			statistics.add(new BasicNameValuePair("trash", Long.toString(trash)));
			statistics.add(new BasicNameValuePair("indexdir", Long.toString(indexdir)));
			statistics.add(new BasicNameValuePair("quota",
					Long.toString(docdir + indexdir + userdir + importdir + exportdir + plugindir + dbdir + logdir)));

			/*
			 * Registration
			 */
			statistics.add(new BasicNameValuePair("reg_name", regName != null ? regName : ""));
			statistics.add(new BasicNameValuePair("reg_email", regEmail != null ? regEmail : ""));
			statistics.add(new BasicNameValuePair("reg_organization", regOrganization != null ? regOrganization : ""));
			statistics.add(new BasicNameValuePair("reg_website", regWebsite != null ? regWebsite : ""));

			uploadStatistics(statistics);

			next();
		} catch (PersistenceException e) {
			throw new TaskException(e.getMessage(), e);
		}
	}

	private String formatDate(Date lastLogin) {
		SimpleDateFormat isoDf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ");
		return lastLogin != null ? isoDf.format(lastLogin) : "";
	}

	private void uploadStatistics(List<NameValuePair> postParams) {
		try {
			// Execute request
			try (CloseableHttpClient httpClient = HttpUtil.getNotValidatingClient(60)) {
				HttpPost post = new HttpPost("http://stat.logicaldoc.com/stats/collect");
				UrlEncodedFormEntity entity = new UrlEncodedFormEntity(postParams, StandardCharsets.UTF_8);
				post.setEntity(entity);

				if (uploadStatistics)
					httpClient.execute(post, new StringHttpClientResponseHandler());
			}
			log.info("Statistics packaged");
		} catch (IOException e) {
			log.warn("Troubles sending the statistics");
			log.debug("Unable to send statistics", e);

		}
	}

	private Date findLastLogin() {
		Date lastLogin = null;
		try {
			lastLogin = documentDAO
					.queryForObject("select max(ld_date) from ld_user_history where ld_deleted=0 and ld_event='"
							+ UserEvent.LOGIN.toString() + "'", Date.class);
		} catch (Exception t) {
			log.warn("Unable to retrieve last login statistics - {}", t.getMessage());
		}
		return lastLogin;
	}

	private Date findLastCreation() {
		Date lastCreation = null;
		try {
			lastCreation = documentDAO
					.queryForObject("select max(ld_date) from ld_history where ld_deleted=0 and ld_event='"
							+ DocumentEvent.STORED + "'", Date.class);
		} catch (Exception t) {
			log.warn("Unable to retrieve last creation statistics - {}", t.getMessage());
		}
		return lastCreation;
	}

	private void calculateAllTenantsFolderStats() throws PersistenceException {
		for (Tenant tenant : tenantDAO.findAll())
			extractFldStats(tenant.getId());
	}

	private void calculateAllTenantsPageStats() throws PersistenceException {
		for (Tenant tenant : tenantDAO.findAll())
			extractPageStats(tenant.getId());
	}

	private void calculateAllTenantsDocsStats() throws PersistenceException {
		for (Tenant tenant : tenantDAO.findAll())
			extractDocStats(tenant.getId());
	}

	private long calculateLogDirSize() {
		long logdir = 0;
		File logsDir = new File(config.getProperty("conf.logdir"));
		if (logsDir.exists())
			logdir = FileUtils.sizeOfDirectory(logsDir);
		return logdir;
	}

	private long calculatePluginDirSize() {
		long plugindir = 0;
		File pluginsDir = PluginRegistry.getPluginsDir();
		plugindir = FileUtils.sizeOfDirectory(pluginsDir);
		return plugindir;
	}

	private long calculateExportDirSize() {
		long exportdir = 0;
		File exportDir = new File(config.getProperty("conf.exportdir"));
		if (exportDir.exists())
			exportdir = FileUtils.sizeOfDirectory(exportDir);
		return exportdir;
	}

	private long calculateImportDirSize() {
		long importdir = 0;
		File importDir = new File(config.getProperty("conf.importdir"));
		if (importDir.exists())
			importdir = FileUtils.sizeOfDirectory(importDir);
		return importdir;
	}

	private long calculateIndexDirSize() {
		long indexdir = 0;
		File indexDir = new File(config.getProperty("index.dir"));
		if (indexDir.exists())
			indexdir = FileUtils.sizeOfDirectory(indexDir);
		return indexdir;
	}

	private long calculateUserDirSize() {
		long userdir = 0;
		File userDir = UserUtil.getUsersDir();
		userdir = FileUtils.sizeOfDirectory(userDir);
		return userdir;
	}

	private long calculateDatabaseSize() {
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
		} catch (Exception t) {
			log.warn("Unable to determine the database size - {}", t.getMessage());
		}

		/*
		 * Fall back to the database dir
		 */
		if (dbdir == 0) {
			File dbDir = new File(config.getProperty("conf.dbdir"));
			if (dbDir.exists())
				dbdir = FileUtils.sizeOfDirectory(dbDir);
		}
		return dbdir;
	}

	private void collectFeatureUsageStats(List<NameValuePair> postParams) {
		long bookmarks = 0;
		try {
			bookmarks = folderDAO.queryForLong("SELECT COUNT(ld_id) FROM ld_bookmark where ld_deleted=0");
		} catch (Exception t) {
			log.warn("Unable to calculate bookmarks statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("bookmarks", Long.toString(bookmarks)));

		long links = 0;
		try {
			links = folderDAO.queryForLong("SELECT COUNT(ld_id) FROM ld_link where ld_deleted=0");
		} catch (Exception t) {
			log.warn("Unable to calculate links statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("links", Long.toString(links)));

		long notes = 0;
		try {
			notes = folderDAO.queryForLong("SELECT COUNT(ld_id) FROM ld_note where ld_deleted=0");
		} catch (Exception t) {
			log.warn("Unable to calculate notes statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("notes", Long.toString(notes)));

		long aliases = 0;
		try {
			aliases = folderDAO.queryForLong("SELECT COUNT(ld_id) FROM ld_document WHERE ld_docref IS NOT NULL");
		} catch (Exception t) {
			log.warn("Unable to calculate aliases statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("aliases", Long.toString(aliases)));

		long tenants = 0;
		try {
			tenants = folderDAO.queryForLong("SELECT COUNT(ld_id) FROM ld_tenant where ld_deleted=0");
		} catch (Exception t) {
			log.warn("Unable to calculate tenants statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("tenants", Long.toString(tenants)));

		long workflowHistories = 0;
		try {
			workflowHistories = folderDAO
					.queryForLong("SELECT COUNT(ld_id) FROM ld_workflowhistory where ld_deleted=0");
		} catch (Exception t) {
			log.warn("Unable to calculate workflow statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("workflow_histories", Long.toString(workflowHistories)));

		long templates = 0;
		try {
			templates = documentDAO.queryForLong("select count(ld_id) from ld_template where ld_deleted=0");
		} catch (Exception t) {
			log.warn("Unable to calculate templates statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("templates", Long.toString(templates)));

		long importFolders = 0;
		try {
			importFolders = documentDAO.queryForLong("select count(ld_id) from ld_importfolder where ld_deleted=0");
		} catch (Exception t) {
			log.warn("Unable to calculate import folders statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("importfolders", Long.toString(importFolders)));

		long stamps = 0;
		try {
			stamps = documentDAO.queryForLong("select count(ld_id) from ld_stamp where ld_deleted=0");
		} catch (Exception t) {
			log.warn("Unable to calculate stamps statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("stamps", Long.toString(stamps)));

		long forms = 0;
		try {
			forms = documentDAO.queryForLong("select count(ld_id) from ld_form where ld_deleted=0");
		} catch (Exception t) {
			log.warn("Unable to calculate forms statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("forms", Long.toString(forms)));

		long reports = 0;
		try {
			reports = documentDAO.queryForLong("select count(ld_id) from ld_report where ld_deleted=0");
		} catch (Exception t) {
			log.warn("Unable to calculate reports statistics - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("reports", Long.toString(reports)));

		// Collect import email account
		long emailAccounts = 0;
		try {
			documentDAO.queryForLong("select count(ld_id) from ld_emailaccount where ld_deleted=0");
		} catch (Exception t) {
			log.warn("Unable to calculate import email accounts - {}", t.getMessage());
		}
		postParams.add(new BasicNameValuePair("emailaccounts", Long.toString(emailAccounts)));

		// Collect calendar events
		long calendarEvents = 0;
		try {
			documentDAO.queryForLong("select count(ld_id) from ld_event where ld_deleted=0");
		} catch (Exception t) {
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
	 *         <li>trash (total size of the trash)</li>
	 *         </ol>
	 * 
	 * @throws PersistenceException error at data layer
	 */
	private long[] extractDocStats(long tenantId) throws PersistenceException {
		long[] stats = new long[8];
		stats[0] = 0;
		try {
			stats[0] = documentDAO.queryForLong(
					"SELECT COUNT(A.ld_id) FROM ld_document A where A.ld_indexed = 0 and A.ld_deleted = 0 "
							+ (tenantId != Tenant.SYSTEM_ID ? AND_A_LD_TENANTID + tenantId : "") + AND_NOT_A_LD_STATUS
							+ AbstractDocument.DOC_ARCHIVED);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[1] = 0;
		try {
			stats[1] = documentDAO.queryForLong(
					"SELECT COUNT(A.ld_id) FROM ld_document A where A.ld_indexed = 1 and A.ld_deleted = 0 "
							+ (tenantId != Tenant.SYSTEM_ID ? AND_A_LD_TENANTID + tenantId : "") + AND_NOT_A_LD_STATUS
							+ AbstractDocument.DOC_ARCHIVED);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[2] = 0;
		try {
			stats[2] = documentDAO.queryForLong("SELECT COUNT(A.ld_id) FROM ld_document A where A.ld_deleted  > 0 "
					+ (tenantId != Tenant.SYSTEM_ID ? AND_A_LD_TENANTID + tenantId : ""));
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
			stats[4] = documentDAO
					.queryForLong("SELECT COUNT(A.ld_id) FROM ld_document A where A.ld_deleted=0 and A.ld_status = "
							+ AbstractDocument.DOC_ARCHIVED
							+ (tenantId != Tenant.SYSTEM_ID ? AND_A_LD_TENANTID + tenantId : ""));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[5] = documentDAO.computeTotalSize(tenantId != Tenant.SYSTEM_ID ? tenantId : null, null, true);

		stats[6] = 0;
		try {
			stats[6] = documentDAO
					.queryForLong("SELECT COUNT(A.ld_id) FROM ld_document A where A.ld_deleted = 0 and A.ld_indexed = "
							+ AbstractDocument.INDEX_SKIP
							+ (tenantId != Tenant.SYSTEM_ID ? AND_A_LD_TENANTID + tenantId : "") + AND_NOT_A_LD_STATUS
							+ AbstractDocument.DOC_ARCHIVED);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[7] = stats[5] - documentDAO.computeTotalSize(tenantId, null, false);

		saveStatistic("notindexeddocs", stats[0], tenantId);
		saveStatistic("indexeddocs", stats[1], tenantId);
		saveStatistic("deleteddocs", stats[2], tenantId);
		saveStatistic("totaldocs", stats[3], tenantId);
		saveStatistic("archiveddocs", stats[4], tenantId);
		saveStatistic("docdir", stats[5], tenantId);
		saveStatistic("notindexabledocs", stats[6], tenantId);
		saveStatistic("trash", stats[7], tenantId);

		return stats;
	}

	/**
	 * Retrieves the pages stats of a specific tenant and saves the results in
	 * the database
	 * 
	 * @param tenantId The tenant Id if the system tenant is used the whole
	 *        stats are computed
	 * @return Ordered list of stats<br/>
	 *         <ol>
	 *         <li>notindexedpages</li>
	 *         <li>indexedpages</li>
	 *         <li>deletedpages</li>
	 *         <li>totalpages</li>
	 *         <li>archivedpages</li>
	 *         <li>notindexablepages</li>
	 *         </ol>
	 * 
	 * @throws PersistenceException Error in the database
	 */
	private long[] extractPageStats(long tenantId) throws PersistenceException {
		long[] stats = new long[6];

		stats[0] = 0;
		try {
			stats[0] = documentDAO.queryForLong(
					"SELECT SUM(A.ld_pages) FROM ld_document A where A.ld_pages > 0 and A.ld_indexed = 0 and A.ld_deleted = 0 "
							+ (tenantId != Tenant.SYSTEM_ID ? AND_A_LD_TENANTID + tenantId : "") + AND_NOT_A_LD_STATUS
							+ AbstractDocument.DOC_ARCHIVED);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[1] = 0;
		try {
			stats[1] = documentDAO.queryForLong(
					"SELECT SUM(A.ld_pages) FROM ld_document A where A.ld_pages > 0 and A.ld_indexed = 1 and A.ld_deleted = 0 "
							+ (tenantId != Tenant.SYSTEM_ID ? AND_A_LD_TENANTID + tenantId : "") + AND_NOT_A_LD_STATUS
							+ AbstractDocument.DOC_ARCHIVED);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[2] = 0;
		try {
			stats[2] = documentDAO.queryForLong("SELECT SUM(A.ld_pages) FROM ld_document A where A.ld_deleted  > 0 "
					+ (tenantId != Tenant.SYSTEM_ID ? AND_A_LD_TENANTID + tenantId : ""));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[3] = 0;
		try {
			stats[3] = documentDAO.queryForLong("SELECT SUM(A.ld_pages) FROM ld_document A where A.ld_pages > 0 "
					+ (tenantId != Tenant.SYSTEM_ID ? AND_A_LD_TENANTID + tenantId : ""));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[4] = 0;
		try {
			stats[4] = documentDAO.queryForLong(
					"SELECT SUM(A.ld_pages) FROM ld_document A where A.ld_status = " + AbstractDocument.DOC_ARCHIVED
							+ (tenantId != Tenant.SYSTEM_ID ? AND_A_LD_TENANTID + tenantId : ""));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[5] = 0;
		try {
			stats[5] = documentDAO.queryForLong("SELECT SUM(A.ld_pages) FROM ld_document A where A.ld_indexed = "
					+ AbstractDocument.INDEX_SKIP + " and A.ld_deleted = 0 "
					+ (tenantId != Tenant.SYSTEM_ID ? AND_A_LD_TENANTID + tenantId : "") + AND_NOT_A_LD_STATUS
					+ AbstractDocument.DOC_ARCHIVED);
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		saveStatistic("notindexedpages", stats[0], tenantId);
		saveStatistic("indexedpages", stats[1], tenantId);
		saveStatistic("deletedpages", stats[2], tenantId);
		saveStatistic("totalpages", stats[3], tenantId);
		saveStatistic("archivedpages", stats[4], tenantId);
		saveStatistic("notindexablepages", stats[5], tenantId);

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
	 * 
	 * @throws PersistenceException Error in the database
	 */
	private long[] extractFldStats(long tenantId) throws PersistenceException {
		long[] stats = new long[4];

		stats[0] = 0;
		try {
			stats[0] = folderDAO.queryForLong(
					"SELECT COUNT(A.ld_id) FROM ld_folder A where A.ld_deleted = 0 and A.ld_id in (select B.ld_folderid FROM ld_document B where B.ld_deleted = 0) "
							+ (tenantId != Tenant.SYSTEM_ID ? AND_A_LD_TENANTID + tenantId : ""));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[1] = 0;
		try {
			stats[1] = folderDAO.queryForLong(
					"SELECT COUNT(A.ld_id) FROM ld_folder A where A.ld_deleted = 0 and A.ld_id not in (select B.ld_folderid FROM ld_document B where B.ld_deleted = 0) "
							+ (tenantId != Tenant.SYSTEM_ID ? AND_A_LD_TENANTID + tenantId : ""));
		} catch (PersistenceException e) {
			log.error(e.getMessage(), e);
		}

		stats[2] = 0;
		try {
			stats[2] = folderDAO.queryForLong("SELECT COUNT(A.ld_id) FROM ld_folder A where A.ld_deleted  > 0 "
					+ (tenantId != Tenant.SYSTEM_ID ? AND_A_LD_TENANTID + tenantId : ""));
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
	 * 
	 * @throws PersistenceException Error in the database
	 */
	protected void saveStatistic(String parameter, Object val, long tenantId) throws PersistenceException {
		Generic gen = genericDAO.findByAlternateKey(STAT, parameter, null, tenantId);
		if (gen == null) {
			gen = new Generic();
			gen.setType(STAT);
			gen.setTenantId(tenantId);
			gen.setSubtype(parameter);
		} else
			genericDAO.initialize(gen);

		switch (val) {
		case Date dateVal -> gen.setDate1(dateVal);
		case String stringVal -> gen.setString1(stringVal);
		case Integer intVal -> gen.setInteger1(intVal.longValue());
		default -> gen.setInteger1(null);
		}

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

	public static void setUserno(String userno) {
		StatsCollector.userno = userno;
	}

	public static void setSID(String sid) {
		StatsCollector.sid = sid;
	}

	public static void setProduct(String product) {
		StatsCollector.product = product;
	}

	public static void setProductName(String productName) {
		StatsCollector.productName = productName;
	}

	void setUploadStatistics(boolean uploadStatistics) {
		this.uploadStatistics = uploadStatistics;
	}

	List<NameValuePair> getStatistics() {
		return statistics;
	}
}