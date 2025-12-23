package com.logicaldoc.core;

import java.io.IOException;
import java.io.InputStream;
import java.sql.SQLException;
import java.util.concurrent.ExecutionException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentDAO;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.folder.Folder;
import com.logicaldoc.core.folder.FolderDAO;
import com.logicaldoc.core.folder.FolderHistory;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ResourceUtil;

import jakarta.annotation.Resource;

/**
 * A super class for those initializers that prepare the environment for
 * execution when the Context has been initialized and the database connected.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public abstract class ContextInitializer implements ApplicationListener<ContextRefreshedEvent> {

	private static final Logger log = LoggerFactory.getLogger(ContextInitializer.class);

	@Resource(name = "ContextProperties")
	protected ContextProperties config;

	@Resource(name = "documentManager")
	protected DocumentManager documentManager;

	@Resource(name = "documentDAO")
	protected DocumentDAO documentDAO;

	@Resource(name = "folderDAO")
	protected FolderDAO folderDAO;

	@Resource(name = "tenantDAO")
	protected TenantDAO tenantDAO;

	/**
	 * Concrete implementations must override this method to put their
	 * initialization logic.
	 * 
	 * @param tenantId Identifier of the tenant to initialize
	 * 
	 * @throws IOException Generic I/O error
	 * @throws SQLException Error in the data layer
	 */
	protected abstract void initialize(long tenantId) throws IOException, SQLException;

	@Override
	public final void onApplicationEvent(ContextRefreshedEvent event) {
		if (!isDbInitialized())
			return;

		try {
			for (Long tenantId : tenantDAO.findAllIds())
				initialize(tenantId);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
	}

	private boolean isDbInitialized() {
		try {
			return documentDAO.queryForLong("select count(*) from ld_user") > 0L;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * Creates a folder
	 * 
	 * @param path The path
	 * @param transaction The current transaction, it is mandatory and used to
	 *        pick the tenant's ID
	 * 
	 * @return The created folder
	 * 
	 * @throws PersistenceException Error in the data layer
	 */
	protected Folder prepareFolder(String path, FolderHistory transaction) throws PersistenceException {
		Folder root = folderDAO.findRoot(transaction.getTenantId());
		root = folderDAO.createPath(root, path, false, transaction);
		return root;
	}

	protected Document prepareDocument(String resourcePath, String documentPath, DocumentHistory transaction)
			throws PersistenceException, InterruptedException, ExecutionException, IOException {
		FolderHistory fHist = new FolderHistory();
		BeanUtils.copyProperties(transaction, fHist, "event");
		fHist.setId(0);
		fHist.setTenantId(transaction.getTenantId());
		Folder target = prepareFolder(FileUtil.getPath(documentPath), fHist);
		String fileName = FileUtil.getName(documentPath);
		Document doc = documentDAO
				.findByFileNameAndParentFolderId(target.getId(), fileName, null, transaction.getTenantId(), null)
				.stream().findFirst().orElse(null);
		if (doc == null) {
			try (InputStream is = ResourceUtil.getInputStream(resourcePath)) {
				Document trainingDoc = new Document();
				trainingDoc.setTenantId(transaction.getTenantId());
				trainingDoc.setFileName(fileName);
				trainingDoc.setFolder(target);
				trainingDoc.setLanguage("en");
				doc = documentManager.create(is, trainingDoc, transaction).get();
			}
		}
		
		documentDAO.initialize(doc);
		
		return doc;
	}
}
