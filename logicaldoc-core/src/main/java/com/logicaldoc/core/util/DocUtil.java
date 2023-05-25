package com.logicaldoc.core.util;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.dao.VersionDAO;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.dao.TenantDAO;
import com.logicaldoc.util.Context;

/**
 * Some utility methods for documents.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class DocUtil {
	protected static Logger log = LoggerFactory.getLogger(DocUtil.class);

	public static String getFileName(Document document, String fileVersion) {
		String fileName = document.getFileName();
		if (StringUtils.isNotEmpty(fileVersion) && !fileVersion.equals(document.getFileVersion())) {
			VersionDAO vDao = (VersionDAO) Context.get().getBean(VersionDAO.class);
			Version ver = vDao.findByFileVersion(document.getId(), fileVersion);
			if (ver != null)
				fileName = ver.getFileName();
		}
		return fileName;
	}

	public static String getTenantName(Document document) {
		String tenantName = "default";
		if (document != null)
			try {
				TenantDAO tenantDao = (TenantDAO) Context.get().getBean(TenantDAO.class);
				Tenant tenant = tenantDao.findById(document.getTenantId());
				tenantName = tenant.getName();
			} catch (Exception t) {
				log.error(t.getMessage());
			}
		return tenantName;
	}

}