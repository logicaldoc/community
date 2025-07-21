package com.logicaldoc.gui.frontend.client.menu;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.layout.VStack;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

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
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setMinWidth(550);
		setMinHeight(350);
		setAutoSize(true);

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

		Label trademark = new Label(I18N.message("copyrights", Session.get().getInfo().getBranding().getProduct(),
				Session.get().getInfo().getBranding().getProduct(), Session.get().getInfo().getBranding().getVendor()));
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

		VStack about = new VStack();
		about.setWidth100();
		about.setMembersMargin(5);
		about.setMargin(4);
		about.setAlign(Alignment.CENTER);
		about.setDefaultLayoutAlign(Alignment.CENTER);
		about.setBackgroundColor("#ffffff");
		about.setMembers(vspacer1, logoImage, productName, version, copyright, trademark, sitelink, maillink, vspacer2);

		Tab aboutTab = new Tab(I18N.message("about"));
		aboutTab.setPane(about);

		Tab changelogTab = new Tab(I18N.message("changelog"));

		TextAreaItem changelogItem = ItemFactory.newTextAreaItem("changelog",
				Session.get().getInfo().getChangelog().replace(
						"================================================================================",
						"============================================================"));
		changelogItem.setWidth("*");
		changelogItem.setShowTitle(false);
		DynamicForm form = new DynamicForm();
		form.setWidth100();
		form.setHeight100();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setItems(changelogItem);
		form.setNumCols(1);
		changelogTab.setPane(form);

		ListGridField name = new ListGridField("name", I18N.message("name"));
		name.setHidden(true);
		name.setWidth(60);

		ListGridField title = new ListGridField("title", I18N.message("legal"));
		title.setWidth("*");

		ListGridField confirmed = new DateListGridField("confirmed", "confirmedon");

		ListGrid legals = new ListGrid();
		legals.setWidth100();
		legals.setHeight100();
		legals.setCanReorderFields(false);
		legals.setCanFreezeFields(false);
		legals.setAutoFetchData(true);
		legals.setSelectionType(SelectionStyle.SINGLE);
		legals.setFilterOnKeypress(false);
		legals.setShowRecordComponents(true);
		legals.setShowRecordComponentsByCell(true);
		legals.setFields(name, title, confirmed);
		legals.setCanResizeFields(true);
		legals.setDataSource(new LegalsDS());
		legals.addDoubleClickHandler(click -> {
			Util.uninstallCloseWindowAlert();
			WindowUtils.openUrl(
					Util.contextPath() + "prev/legal.jsp?legal=" + legals.getSelectedRecord().getAttribute("name"),
					"_blank");
			Util.installCloseWindowAlert();
		});

		Tab legalsTab = new Tab(I18N.message("legals"));
		legalsTab.setPane(legals);

		TabSet tabs = new TabSet();
		tabs.setWidth100();
		tabs.setHeight100();

		if (Session.get().getUser().isLegals())
			tabs.setTabs(aboutTab, changelogTab, legalsTab);
		else
			tabs.setTabs(aboutTab, changelogTab);

		addItem(tabs);
	}
}