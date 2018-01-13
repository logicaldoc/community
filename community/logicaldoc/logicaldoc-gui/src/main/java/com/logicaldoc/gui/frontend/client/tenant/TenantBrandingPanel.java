package com.logicaldoc.gui.frontend.client.tenant;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIBranding;
import com.logicaldoc.gui.common.client.beans.GUITenant;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.util.WindowUtils;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.ValueCallback;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows document's standard properties and read-only data
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 6.0
 */
public class TenantBrandingPanel extends HLayout {

	private DynamicForm form = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private GUITenant tenant;

	private ChangedHandler changedHandler;

	private HLayout layout = new HLayout();

	private VLayout imagesPanel = new VLayout();

	private SelectItem skin = null;

	private ListGrid grid;

	public TenantBrandingPanel(final GUITenant tenant, final ChangedHandler changedHandler) {
		if (tenant == null) {
			setMembers(TenantsPanel.SELECT_TENANT);
		} else {
			this.tenant = tenant;
			this.changedHandler = changedHandler;
			setWidth100();
			setHeight100();

			ToolStrip toolStrip = new ToolStrip();
			toolStrip.setHeight(20);
			toolStrip.setWidth100();
			toolStrip.addSpacer(2);
			ToolStripButton imp = new ToolStripButton();
			imp.setTitle(I18N.message("iimport"));
			imp.setDisabled(changedHandler == null);
			toolStrip.addButton(imp);
			imp.addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					PackageUploader uploader = new PackageUploader(TenantBrandingPanel.this);
					uploader.show();
				}
			});
			ToolStripButton export = new ToolStripButton();
			export.setTitle(I18N.message("export"));
			toolStrip.addButton(export);
			export.addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					WindowUtils.openUrl(Util.contextPath() + "exportbranding?tenantId=" + tenant.getId());
				}
			});

			ToolStripButton customCss = new ToolStripButton(I18N.message("customcss"));
			customCss.setAutoDraw(true);
			customCss.addClickHandler(new ClickHandler() {

				@Override
				public void onClick(ClickEvent event) {
					TextAreaItem item = ItemFactory.newTextAreaItem("css", I18N.message("customcss"), tenant
							.getBranding().getCss());
					item.setHeight(com.google.gwt.user.client.Window.getClientHeight() - 100);
					LD.askForValue(I18N.message("customcss"), I18N.message("entercustomcss"), tenant.getBranding()
							.getCss(), item, com.google.gwt.user.client.Window.getClientWidth() - 50,
							new ValueCallback() {

								@Override
								public void execute(String value) {
									tenant.getBranding().setCss(value);
									if (changedHandler != null)
										changedHandler.onChanged(null);
								}
							});
				}
			});

			skin = ItemFactory.newSkinSelector();
			skin.addChangedHandler(changedHandler);
			toolStrip.addSeparator();
			toolStrip.addFormItem(skin);

			if (Feature.enabled(Feature.BRANDING_FULL))
				toolStrip.addButton(customCss);

			toolStrip.addFill();

			VLayout panel = new VLayout();
			panel.setMembersMargin(2);
			panel.setMembers(toolStrip, layout);
			addMember(panel);
			refresh();
		}
	}

	public void refresh() {
		boolean readonly = (changedHandler == null);
		vm.clearValues();
		vm.clearErrors(false);
		vm = new ValuesManager();

		if (form != null)
			form.destroy();

		if (layout.contains(form))
			layout.removeMember(form);
		if (layout.contains(imagesPanel))
			layout.removeMember(imagesPanel);

		skin.setValue(tenant.getBranding().getSkin());

		form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.TOP);

		boolean fullBranding = Feature.enabled(Feature.BRANDING_FULL);

		TextItem product = ItemFactory.newTextItem("product", "product", tenant.getBranding().getProduct());
		product.setWidth(180);
		product.setDisabled(readonly || !fullBranding);
		product.addChangedHandler(changedHandler);

		TextItem productName = ItemFactory.newTextItem("productName", "productname", tenant.getBranding()
				.getProductName());
		productName.setWidth(180);
		productName.setDisabled(readonly || !fullBranding);
		productName.addChangedHandler(changedHandler);

		TextItem vendor = ItemFactory.newTextItem("vendor", "vendor", tenant.getBranding().getVendor());
		vendor.setWidth(180);
		vendor.setDisabled(readonly || !fullBranding);
		vendor.addChangedHandler(changedHandler);

		TextItem address = ItemFactory.newTextItem("address", "address", tenant.getBranding().getVendorAddress());
		address.setWidth(180);
		address.setDisabled(readonly || !fullBranding);
		address.addChangedHandler(changedHandler);

		TextItem postalCode = ItemFactory.newTextItem("postalCode", "postalcode", tenant.getBranding().getVendorCap());
		postalCode.setWidth(180);
		postalCode.setDisabled(readonly || !fullBranding);
		postalCode.addChangedHandler(changedHandler);

		TextItem city = ItemFactory.newTextItem("city", "city", tenant.getBranding().getVendorCity());
		city.setWidth(180);
		city.setDisabled(readonly || !fullBranding);
		city.addChangedHandler(changedHandler);

		TextItem country = ItemFactory.newTextItem("country", "country", tenant.getBranding().getVendorCountry());
		country.setWidth(180);
		country.setDisabled(readonly || !fullBranding);
		country.addChangedHandler(changedHandler);

		TextItem support = ItemFactory.newEmailItem("support", "support", false);
		support.setValue(tenant.getBranding().getSupport());
		support.setDisabled(readonly || !fullBranding);
		support.addChangedHandler(changedHandler);

		TextItem sales = ItemFactory.newEmailItem("sales", "sales", false);
		sales.setValue(tenant.getBranding().getSales());
		sales.setEndRow(true);
		sales.setDisabled(readonly || !fullBranding);
		sales.addChangedHandler(changedHandler);

		TextItem webSite = ItemFactory.newTextItem("website", "website", tenant.getBranding().getUrl());
		webSite.setDisabled(readonly || !fullBranding);
		webSite.setColSpan(2);
		webSite.setWidth(360);
		webSite.addChangedHandler(changedHandler);

		TextItem help = ItemFactory.newTextItem("help", "help", tenant.getBranding().getHelp());
		help.setDisabled(readonly || !fullBranding);
		help.setColSpan(2);
		help.setWidth(360);
		help.addChangedHandler(changedHandler);

		TextItem bugs = ItemFactory.newTextItem("bugs", "bugs", tenant.getBranding().getBugs());
		bugs.setDisabled(readonly || !fullBranding);
		bugs.setColSpan(2);
		bugs.setWidth(360);
		bugs.addChangedHandler(changedHandler);

		TextItem forum = ItemFactory.newTextItem("forum", "forum", tenant.getBranding().getForum());
		forum.setDisabled(readonly || !fullBranding);
		forum.setColSpan(2);
		forum.setWidth(360);
		forum.addChangedHandler(changedHandler);

		form.setItems(product, productName, vendor, address, postalCode, city, country, support, sales, webSite, help,
				forum, bugs);

		imagesPanel.setMembers(prepareImagesGrid());

		layout.setMembersMargin(4);
		layout.setMembers(form, imagesPanel);
	}

	private ListGrid prepareImagesGrid() {
		grid = new ListGrid();
		grid.setSelectionType(SelectionStyle.SINGLE);
		grid.setCellPadding(2);
		grid.setAutoFetchData(true);
		grid.setShowRecordComponents(true);
		grid.setShowRecordComponentsByCell(true);
		grid.setHeight100();

		ListGridField name = new ListGridField("name", I18N.getAttributeLabel("image"), 100);
		name.setCanFilter(false);
		name.setCanSort(false);

		ListGridField image = new ListGridField("image", " ", 300);
		image.setWidth("*");
		image.setCanFilter(false);
		image.setCanSort(false);
		image.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				try {
					String html = "";
					String name = record.getAttributeAsString("name");
					if (name.equals("logo"))
						html = tenant.getBranding().getLogoSrc();
					else if (name.equals("logo_head"))
						html = tenant.getBranding().getLogoHeadSrc();
					else if (name.equals("logo_oem"))
						html = tenant.getBranding().getLogoOemSrc();
					else if (name.equals("logo_head_oem"))
						html = tenant.getBranding().getLogoHeadOemSrc();
					else if (name.equals("banner"))
						html = tenant.getBranding().getBannerSrc();
					else if (name.equals("favicon"))
						html = tenant.getBranding().getFaviconSrc();

					if (html != null && !html.isEmpty()) {

						html = "<img src='" + html + "' style='margin-top:4px; margin-bottom:4px;'/>";

						// In chase of header logos, show the banner below
						if (name.contains("_head")) {
							html = "<div style=\" background-image: url('" + tenant.getBranding().getBannerSrc()
									+ "'); width:100%; margin-top:4px; margin-bottom:4px; \">" + html + "</div>";
						} else {
							html = "<div style='width:100%; margin-top:4px; margin-bottom:4px;'>" + html + "</div>";
						}
					}
					return html;
				} catch (Throwable e) {
					return "";
				}
			}
		});

		grid.setFields(name, image);

		List<ListGridRecord> records = new ArrayList<ListGridRecord>();

		ListGridRecord rec = new ListGridRecord();
		rec.setAttribute("name", "logo");
		rec.setAttribute("image", tenant.getBranding().getLogoSrc());
		records.add(rec);
		rec = new ListGridRecord();
		rec.setAttribute("name", "logo_head");
		rec.setAttribute("image", tenant.getBranding().getLogoHeadSrc());
		records.add(rec);
		rec = new ListGridRecord();
		rec.setAttribute("name", "banner");
		rec.setAttribute("image", tenant.getBranding().getBannerSrc());
		records.add(rec);

		rec = new ListGridRecord();
		rec.setAttribute("name", "logo_oem");
		rec.setAttribute("image", tenant.getBranding().getLogoOemSrc());
		records.add(rec);
		rec = new ListGridRecord();
		rec.setAttribute("name", "logo_head_oem");
		rec.setAttribute("image", tenant.getBranding().getLogoHeadOemSrc());
		records.add(rec);
		rec = new ListGridRecord();
		rec.setAttribute("name", "favicon");
		rec.setAttribute("image", tenant.getBranding().getFaviconSrc());
		records.add(rec);
		grid.setRecords(records.toArray(new ListGridRecord[0]));

		if (changedHandler != null)
			grid.addCellContextClickHandler(new CellContextClickHandler() {
				@Override
				public void onCellContextClick(CellContextClickEvent event) {
					showImagesContextMenu();
					event.cancel();
				}
			});

		return grid;
	}

	private void showImagesContextMenu() {
		final ListGridRecord record = grid.getSelectedRecord();
		final String name = record.getAttributeAsString("name");

		if (!Feature.enabled(Feature.BRANDING_LOGO) && !"logo_oem".equals(name) && !"logo_head_oem".equals(name))
			return;

		Menu contextMenu = new Menu();
		MenuItem reset = new MenuItem();
		reset.setTitle(I18N.message("reset"));
		reset.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				GUIBranding model = new GUIBranding();

				String image = "";
				if ("logo".equals(name)) {
					tenant.getBranding().setLogoSrc(model.getLogoSrc());
					image = model.getLogo();
				} else if ("logo_head".equals(name)) {
					tenant.getBranding().setLogoHeadSrc(model.getLogoHeadSrc());
					image = model.getLogoHead();
				} else if ("logo_oem".equals(name)) {
					tenant.getBranding().setLogoOemSrc(model.getLogoOemSrc());
					image = model.getLogoOem();
				} else if ("logo_head_oem".equals(name)) {
					tenant.getBranding().setLogoHeadOemSrc(model.getLogoHeadOemSrc());
					image = model.getLogoHeadOem();
				} else if ("favicon".equals(name)) {
					tenant.getBranding().setFaviconSrc(model.getFaviconSrc());
					image = model.getFavicon();
				} else if ("banner".equals(name)) {
					tenant.getBranding().setBannerSrc(model.getBannerSrc());
					image = model.getBanner();
				}

				updateImage(name, image);
			}
		});

		MenuItem upload = new MenuItem();
		upload.setTitle(I18N.message("uploadnewimage"));
		upload.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				ImageUploader uploader = new ImageUploader(record.getAttributeAsString("name"),
						TenantBrandingPanel.this);
				uploader.show();
			}
		});

		contextMenu.setItems(upload, reset);
		contextMenu.showContextMenu();
	}

	void updateImage(String imageName, String imageContent) {
		String content = imageContent;
		if (content.startsWith(GUIBranding.SRC_PREFIX))
			content = GUIBranding.SRC_PREFIX + imageContent;

		if (imageName.equals("logo"))
			tenant.getBranding().setLogoSrc(content);
		else if (imageName.equals("logo_head"))
			tenant.getBranding().setLogoHeadSrc(content);
		else if (imageName.equals("logo_oem"))
			tenant.getBranding().setLogoOemSrc(content);
		else if (imageName.equals("logo_head_oem"))
			tenant.getBranding().setLogoHeadOemSrc(content);
		else if (imageName.equals("banner"))
			tenant.getBranding().setBannerSrc(content);
		else if (imageName.equals("favicon"))
			tenant.getBranding().setFaviconSrc(content);

		Record record = null;

		// Find the record the corresponds to the given document
		Record[] records = grid.getRecords();
		for (Record rec : records)
			if (rec.getAttribute("name").equals(imageName))
				record = rec;

		if (record != null) {
			record.setAttribute("image", content);
			grid.invalidateRecordComponents();
			grid.refreshRecordComponent(grid.getRecordIndex(record));
			grid.redraw();
		}

		if (changedHandler != null)
			changedHandler.onChanged(null);
	}

	public boolean validate() {
		vm.validate();
		if (!vm.hasErrors()) {
			if (Feature.enabled(Feature.BRANDING_FULL)) {
				tenant.getBranding().setSkin(skin.getValueAsString());
				tenant.getBranding().setProduct(vm.getValueAsString("product"));
				tenant.getBranding().setProductName(vm.getValueAsString("productName"));
				tenant.getBranding().setVendor(vm.getValueAsString("vendor"));
				tenant.getBranding().setVendorAddress(vm.getValueAsString("address"));
				tenant.getBranding().setVendorCap(vm.getValueAsString("postalCode"));
				tenant.getBranding().setVendorCity(vm.getValueAsString("city"));
				tenant.getBranding().setVendorCountry(vm.getValueAsString("country"));
				tenant.getBranding().setUrl(vm.getValueAsString("website"));
				tenant.getBranding().setBugs(vm.getValueAsString("bugs"));
				tenant.getBranding().setForum(vm.getValueAsString("forum"));
				tenant.getBranding().setHelp(vm.getValueAsString("help"));
				tenant.getBranding().setSupport(vm.getValueAsString("support"));
				tenant.getBranding().setSales(vm.getValueAsString("sales"));
			}
		}

		return !vm.hasErrors();
	}

	public void update(GUIBranding branding) {
		tenant.setBranding(branding);
		refresh();
		if (changedHandler != null)
			changedHandler.onChanged(null);
	}
}