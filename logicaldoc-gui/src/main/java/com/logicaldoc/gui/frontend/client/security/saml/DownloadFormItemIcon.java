package com.logicaldoc.gui.frontend.client.security.saml;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;

/**
 * An icon to be used to download a resource of Saml
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9
 */
public class DownloadFormItemIcon extends FormItemIcon {

	/**
	 * The constructor.
	 * 
	 * @param url the url to invoke
	 */
	public DownloadFormItemIcon(String url) {
		setPrompt(I18N.message("download"));
		setSrc("[SKIN]/download.svg");
		setWidth(16);
		setHeight(16);
		addFormItemClickHandler(event -> Util.download(url));
	}
}
