package com.logicaldoc.gui.frontend.client.menu;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VStack;

/**
 * This is the about dialog.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class AboutDialog extends Window {

	/**
	 * Public constructor
	 */
	public AboutDialog() {
		super();

		addCloseClickHandler(event -> destroy());

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("about") + " " + Session.get().getInfo().getBranding().getProductName());
		setWidth(400);
		setPadding(2);
		setAutoSize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		HTMLPane vspacer1 = new HTMLPane();
		vspacer1.setContents("<div>&nbsp;</div>");
		vspacer1.setPixelSize(100, 5);
		vspacer1.setOverflow(Overflow.HIDDEN);

		Img logoImage = ItemFactory.newBrandImg("logo.png", Session.get().getInfo());
		logoImage.setWidth(205);
		logoImage.setHeight(40);

		Label productName = new Label(Session.get().getInfo().getBranding().getProductName());
		productName.setWrap(false);
		productName.setHeight(10);
		productName.setAlign(Alignment.CENTER);

		Label version = new Label(I18N.message("version") + " " + Session.get().getInfo().getRelease());
		version.setWrap(false);
		version.setHeight(10);
		version.setAlign(Alignment.CENTER);

		Label copyright = new Label("&copy; " + Session.get().getInfo().getYear() + " "
				+ Session.get().getInfo().getBranding().getVendor());
		copyright.setWrap(false);
		copyright.setHeight(20);
		copyright.setAlign(Alignment.CENTER);

		Label trademark = new Label(I18N.message("copyrights",
				new String[] { Session.get().getInfo().getBranding().getProduct(),
						Session.get().getInfo().getBranding().getProduct(),
						Session.get().getInfo().getBranding().getVendor() }));
		trademark.setWidth("80%");
		trademark.setHeight(40);
		trademark.setAlign(Alignment.CENTER);

		// Prepare the website link
		String wsurl = Session.get().getInfo().getBranding().getUrl();

		String htmlUrl = "";
		if (Session.get().getInfo().getBranding().getUrl() != null
				&& !"-".equals(Session.get().getInfo().getBranding().getUrl()))
			htmlUrl = "<div style='text-align: center;'><a href='" + wsurl + "' target='_blank'>" + wsurl
					+ "</a></div>";
		HTMLPane sitelink = new HTMLPane();
		sitelink.setContents(htmlUrl);
		sitelink.setPixelSize(300, 16);
		sitelink.setAlign(Alignment.CENTER);
		sitelink.setLayoutAlign(Alignment.CENTER);

		// Prepare the support link
		String support = Session.get().getInfo().getBranding().getSupport();
		String htmlSupp = "<div style='text-align: center;'><a href='mailto:" + support + "'>" + support + "</a></div>";
		HTMLPane maillink = new HTMLPane();
		maillink.setContents(htmlSupp);
		maillink.setPixelSize(300, 16);
		maillink.setAlign(Alignment.CENTER);
		maillink.setLayoutAlign(Alignment.CENTER);
		maillink.setVisible(Feature.enabled(Feature.TECHNICAL_SUPPORT));

		HTMLPane vspacer2 = new HTMLPane();
		vspacer2.setContents("<div>&nbsp;</div>");
		vspacer2.setPixelSize(100, 10);
		vspacer2.setOverflow(Overflow.HIDDEN);

		VStack content = new VStack();
		content.setWidth("100%");
		content.setMembersMargin(5);
		content.setMargin(4);
		content.setAlign(Alignment.CENTER);
		content.setDefaultLayoutAlign(Alignment.CENTER);
		content.setBackgroundColor("#ffffff");
		content.setMembers(vspacer1, logoImage, productName, version, copyright, trademark, sitelink, maillink,
				vspacer2);

		addItem(content);
	}
}