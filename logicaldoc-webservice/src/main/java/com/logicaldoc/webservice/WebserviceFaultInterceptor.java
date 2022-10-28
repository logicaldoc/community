package com.logicaldoc.webservice;

import org.apache.cxf.interceptor.Fault;
import org.apache.cxf.interceptor.FaultOutInterceptor;
import org.apache.cxf.message.Message;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.HibernatePersistentObjectDAO;

/**
 * Logs the errors into the LogicalDOC main log
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public class WebserviceFaultInterceptor extends FaultOutInterceptor {

	private Logger log = LoggerFactory.getLogger(HibernatePersistentObjectDAO.class);

	public WebserviceFaultInterceptor() {
	}

	@Override
	public void handleMessage(Message message) {
		// Original SoapFault
		final Fault f = (Fault) message.getContent(Exception.class);

		if (f.getCause() != null)
			log.error(f.getCause().getMessage(), f.getCause());
		else
			log.error(f.getMessage(), f);
	}
}