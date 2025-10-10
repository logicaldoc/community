package com.logicaldoc.core.util;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.Version;
import com.logicaldoc.core.document.VersionDAO;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.util.spring.Context;

/**
 * Some utility methods for documents.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class DocUtil {
	private static final Logger log = LoggerFactory.getLogger(DocUtil.class);

	private DocUtil() {
	}

	public static String getFileName(Document document, String fileVersion) {
		String fileName = document.getFileName();
		if (StringUtils.isNotEmpty(fileVersion) && !fileVersion.equals(document.getFileVersion())) {
			VersionDAO vDao = Context.get(VersionDAO.class);
			try {
				Version ver = vDao.findByFileVersion(document.getId(), fileVersion);
				if (ver != null)
					fileName = ver.getFileName();
			} catch (PersistenceException e) {
				log.error(e.getMessage(), e);
			}
		}
		return fileName;
	}

	public static String getTenantName(Document document) {
		String tenantName = "default";
		if (document != null)
			try {
				TenantDAO tenantDao = Context.get(TenantDAO.class);
				Tenant tenant = tenantDao.findById(document.getTenantId());
				tenantName = tenant.getName();
			} catch (Exception t) {
				log.error(t.getMessage());
			}
		return tenantName;
	}

}