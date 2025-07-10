package com.logicaldoc.webdav.resource;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jakarta.servlet.http.HttpServletResponse;

import org.apache.jackrabbit.webdav.DavCompliance;
import org.apache.jackrabbit.webdav.DavException;
import org.apache.jackrabbit.webdav.DavResource;
import org.apache.jackrabbit.webdav.DavResourceLocator;
import org.apache.jackrabbit.webdav.DavServletResponse;
import org.apache.jackrabbit.webdav.property.DavProperty;
import org.apache.jackrabbit.webdav.property.DavPropertyName;
import org.apache.jackrabbit.webdav.property.HrefProperty;
import org.apache.jackrabbit.webdav.version.DeltaVConstants;
import org.apache.jackrabbit.webdav.version.DeltaVResource;
import org.apache.jackrabbit.webdav.version.OptionsInfo;
import org.apache.jackrabbit.webdav.version.OptionsResponse;
import org.apache.jackrabbit.webdav.version.report.Report;
import org.apache.jackrabbit.webdav.version.report.ReportInfo;
import org.apache.jackrabbit.webdav.version.report.ReportType;
import org.apache.jackrabbit.webdav.version.report.SupportedReportSetProperty;

import com.logicaldoc.webdav.resource.model.Resource;
import com.logicaldoc.webdav.session.WebdavSession;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.webdav.simple.DeltaVResourceImpl}
 * 
 * @author Sebastian Wenzky
 */
public class DeltaVResourceImpl extends DavResourceImpl implements DeltaVResource, Serializable {

	private static final long serialVersionUID = 1L;

	protected transient SupportedReportSetProperty supportedReports = new SupportedReportSetProperty();

	public DeltaVResourceImpl(DavResourceLocator locator, DavResourceFactory factory, WebdavSession session,
			ResourceConfiguration config, Resource resource) throws DavException {
		super(locator, factory, session, config, resource);
		initSupportedReports();
	}

	public DeltaVResourceImpl(DavResourceLocator locator, DavResourceFactory factory, WebdavSession session,
			ResourceConfiguration config) throws DavException {
		super(locator, factory, session, config);
		initSupportedReports();
	}

	public DeltaVResourceImpl(DavResourceLocator locator, DavResourceFactory factory, WebdavSession session,
			ResourceConfiguration config, boolean isCollection) throws DavException {
		super(locator, factory, session, config, isCollection);
		initSupportedReports();
	}

	@Override
	public String getComplianceClass() {
		return DavCompliance.concatComplianceClasses(new String[] { DavCompliance._1_, DavCompliance.VERSION_CONTROL,
				DavCompliance.VERSION_HISTORY, DavCompliance.LABEL });
	}

	public OptionsResponse getOptionResponse(OptionsInfo optionsInfo) {
		OptionsResponse oR = null;
		if (optionsInfo != null) {
			oR = new OptionsResponse();
			// currently only DAV:version-history-collection-set is supported
			if (optionsInfo.containsElement(DeltaVConstants.XML_VH_COLLECTION_SET, DeltaVConstants.NAMESPACE)) {
				String[] hrefs = new String[] { getLocatorFromNodePath().getHref(true) };
				oR.addEntry(DeltaVConstants.XML_VH_COLLECTION_SET, DeltaVConstants.NAMESPACE, hrefs);
			}
		}
		return oR;
	}

	public Report getReport(ReportInfo reportInfo) throws DavException {
		if (reportInfo == null) {
			throw new DavException(HttpServletResponse.SC_BAD_REQUEST,
					"A REPORT request must provide a valid XML request body.");
		}
		if (!exists()) {
			throw new DavException(HttpServletResponse.SC_NOT_FOUND);
		}

		if (supportedReports.isSupportedReport(reportInfo)) {
			return ReportType.getType(reportInfo).createReport(this, reportInfo);
		} else {
			throw new DavException(DavServletResponse.SC_UNPROCESSABLE_ENTITY,
					"Unkown report " + reportInfo.getReportName() + "requested.");
		}
	}

	public void addWorkspace(DavResource workspace) throws DavException {
		throw new DavException(HttpServletResponse.SC_FORBIDDEN);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public DavResource[] getReferenceResources(DavPropertyName hrefPropertyName) throws DavException {
		DavProperty prop = getProperty(hrefPropertyName);
		List resources = new ArrayList();
		if (prop instanceof HrefProperty hp) {
			// process list of hrefs
			List hrefs = hp.getHrefs();
			for (Iterator iter = hrefs.iterator(); iter.hasNext();) {
				String href = (String) iter.next();
				DavResourceLocator locator = getLocator().getFactory().createResourceLocator(getLocator().getPrefix(),
						href);
				resources.add(createResourceFromLocator(locator));
			}
		} else {
			throw new DavException(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		}
		return (DavResource[]) resources.toArray(new DavResource[0]);
	}

	protected DavResourceLocator getLocatorFromNodePath() {
		return getLocator().getFactory().createResourceLocator(getLocator().getPrefix(), "/", "", false);
	}

	protected DavResource createResourceFromLocator(DavResourceLocator loc) throws DavException {
		return getFactory().createResource(loc, getSession());
	}

	protected void initSupportedReports() {
		if (exists()) {
			supportedReports.addReportType(ReportType.EXPAND_PROPERTY);
			if (isCollection()) {
				supportedReports.addReportType(ReportType.LOCATE_BY_HISTORY);
			}
		}
	}

	@Override
	protected void initProperties() {
		if (!propsInitialized) {
			super.initProperties();
			if (exists()) {
				properties.add(supportedReports);
			}
		}
	}
}
