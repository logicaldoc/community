package com.logicaldoc.cmis;

import java.math.BigInteger;
import java.util.Map;

import jakarta.servlet.http.HttpServletRequest;

import org.apache.chemistry.opencmis.commons.impl.server.AbstractServiceFactory;
import org.apache.chemistry.opencmis.commons.server.CallContext;
import org.apache.chemistry.opencmis.commons.server.CmisService;
import org.apache.chemistry.opencmis.server.support.wrapper.ConformanceCmisServiceWrapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.security.Session;
import com.logicaldoc.core.security.SessionManager;
import com.logicaldoc.util.Context;

/**
 * CMIS Service factory
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5.1
 */
public class ServiceFactory extends AbstractServiceFactory {

	public static final String KEY_REPO_ID = "cmis-repoId";

	private static final BigInteger DEFAULT_MAX_ITEMS_TYPES = BigInteger.valueOf(50);

	private static final BigInteger DEFAULT_MAX_ITEMS_OBJECTS = BigInteger.valueOf(200);

	private static final BigInteger DEFAULT_DEPTH_OBJECTS = BigInteger.valueOf(10);

	private static final Logger log = LoggerFactory.getLogger(ServiceFactory.class);

	public ServiceFactory() {
		super();
	}

	@Override
	public CmisService getService(CallContext context) {
		Session session = SessionManager.get()
				.getSession((HttpServletRequest) context.get(CallContext.HTTP_SERVLET_REQUEST));

		CmisService wrapperService = null;
		if (session != null) {
			if (context.getRepositoryId() != null)
				session.getDictionary().put(KEY_REPO_ID, context.getRepositoryId());
			log.debug("Using session {} for user {}", session.getSid(), session.getUsername());
			wrapperService = new ConformanceCmisServiceWrapper(new LDCmisService(context, session.getSid()),
					DEFAULT_MAX_ITEMS_TYPES,
					BigInteger.valueOf(Context.get().getProperties().getInt("cmis.maxitems", 200)),
					DEFAULT_MAX_ITEMS_OBJECTS, DEFAULT_DEPTH_OBJECTS);

		} else {
			log.warn("No session was found for this request");
		}
		return wrapperService;
	}

	@Override
	public void init(Map<String, String> parameters) {
		// Do nothing
	}

	@Override
	public void destroy() {
		// Do nothing
	}
}