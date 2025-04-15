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
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.AutoFitWidthApproach;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Shows document's standard properties and read-only data
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class TenantBrandingPanel extends HLayout {

	private static final String FAVICON = "favicon";

	private static final String LOGO_HEAD_OEM = "logo_head_oem";

	private static final String LOGO_OEM = "logo_oem";

	private static final String NAME = "name";

	private static final String BANNER = "banner";

	private static final String LOGO_MENU = "logo_menu";

	private static final String LOGO_HEAD = "logo_head";

	private static final String IMAGE = "image";

	private static final String SALES = "sales";

	private static final String SUPPORT = "support";

	private DynamicForm form = new DynamicForm();

	private ValuesManager vm = new ValuesManager();

	private GUITenant tenant;

	private ChangedHandler changedHandler;

	private HLayout layout = new HLayout();

	private VLayout imagesPanel = new VLayout();

	private SelectItem skin = null;

	private ListGrid grid;

	public TenantBrandingPanel(GUITenant tenant, final ChangedHandler changedHandler) {
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

			addImportButton(changedHandler, toolStrip);

			addExportButton(tenant, toolStrip);

			addResetButton(tenant, toolStrip);

			toolStrip.addSeparator();

			addSkinSelector(changedHandler, toolStrip);

			addCssButton(changedHandler, toolStrip);

			addHeadButton(changedHandler, toolStrip);

			addTopButton(changedHandler, toolStrip);

			addBottomButton(changedHandler, toolStrip);

			addFooterButton(changedHandler, toolStrip);

			toolStrip.addFill();

			VLayout panel = new VLayout();
			panel.setMembersMargin(2);
			panel.setMembers(toolStrip, layout);
			addMember(panel);
			refresh();
		}
	}

	private void addSkinSelector(final ChangedHandler changedHandler, ToolStrip toolStrip) {
		skin = ItemFactory.newSkinSelector();
		skin.addChangedHandler(changedHandler);
		toolStrip.addFormItem(skin);
	}

	private void addResetButton(GUITenant tenant, ToolStrip toolStrip) {
		ToolStripButton reset = new ToolStripButton();
		reset.setTitle(I18N.message("reset"));
		toolStrip.addButton(reset);
		reset.addClickHandler(event -> {
			GUIBranding newBranding = new GUIBranding();
			if (Feature.enabled(Feature.QUOTAS))
				newBranding.setProductName("LogicalDOC Enterprise");
			else
				newBranding.setProductName("LogicalDOC Business");
			tenant.setBranding(newBranding);
			refresh();
		});
	}

	private void addExportButton(GUITenant tenant, ToolStrip toolStrip) {
		ToolStripButton export = new ToolStripButton();
		export.setTitle(I18N.message("export"));
		toolStrip.addButton(export);
		export.addClickHandler(
				event -> Util.download(Util.contextPath() + "exportbranding?tenantId=" + tenant.getId()));
	}

	private void addImportButton(final ChangedHandler changedHandler, ToolStrip toolStrip) {
		ToolStripButton importButton = new ToolStripButton();
		importButton.setTitle(I18N.message("iimport"));
		importButton.setDisabled(changedHandler == null);
		toolStrip.addButton(importButton);
		importButton.addClickHandler(event -> new BrandingPackageUploader(TenantBrandingPanel.this).show());
	}

	private void addFooterButton(ChangedHandler changedHandler, ToolStrip toolStrip) {
		ToolStripButton footer = new ToolStripButton(I18N.message("footer"));
		footer.addClickHandler(event -> {
			TextAreaItem item = ItemFactory.newTextAreaItemForAutomation("footer", I18N.message("customfooter"),
					tenant.getBranding().getFooter(), null, true);
			item.setHeight(com.google.gwt.user.client.Window.getClientHeight() - 100);
			LD.askForValue(I18N.message("customfooter"), I18N.message("entercustomfooter"),
					tenant.getBranding().getFooter(), item, com.google.gwt.user.client.Window.getClientWidth() - 50,
					value -> {
						tenant.getBranding().setFooter(value);
						if (changedHandler != null)
							changedHandler.onChanged(null);
					});
		});
		if (Feature.enabled(Feature.BRANDING_FULL))
			toolStrip.addButton(footer);
	}

	private void addBottomButton(ChangedHandler changedHandler, ToolStrip toolStrip) {
		ToolStripButton bottom = new ToolStripButton(I18N.message("bottom"));
		bottom.addClickHandler(event -> {
			TextAreaItem item = ItemFactory.newTextAreaItemForAutomation("bottom", I18N.message("custombottom"),
					tenant.getBranding().getBottom(), null, true);
			item.setHeight(com.google.gwt.user.client.Window.getClientHeight() - 100);
			LD.askForValue(I18N.message("custombottom"), I18N.message("entercustombottom"),
					tenant.getBranding().getBottom(), item, com.google.gwt.user.client.Window.getClientWidth() - 50,
					value -> {
						tenant.getBranding().setBottom(value);
						if (changedHandler != null)
							changedHandler.onChanged(null);
					});
		});
		if (Feature.enabled(Feature.BRANDING_FULL))
			toolStrip.addButton(bottom);
	}

	private void addTopButton(ChangedHandler changedHandler, ToolStrip toolStrip) {
		ToolStripButton top = new ToolStripButton(I18N.message("top"));
		top.addClickHandler(event -> {
			TextAreaItem item = ItemFactory.newTextAreaItemForAutomation("top", I18N.message("customtop"),
					tenant.getBranding().getTop(), null, true);
			item.setHeight(com.google.gwt.user.client.Window.getClientHeight() - 100);
			LD.askForValue(I18N.message("customtop"), I18N.message("entercustomtop"), tenant.getBranding().getTop(),
					item, com.google.gwt.user.client.Window.getClientWidth() - 50, value -> {
						tenant.getBranding().setTop(value);
						if (changedHandler != null)
							changedHandler.onChanged(null);
					});
		});
		if (Feature.enabled(Feature.BRANDING_FULL))
			toolStrip.addButton(top);
	}

	private void addHeadButton(ChangedHandler changedHandler, ToolStrip toolStrip) {
		ToolStripButton head = new ToolStripButton(I18N.message("head"));
		head.addClickHandler(event -> {
			TextAreaItem item = ItemFactory.newTextAreaItemForAutomation("head", I18N.message("customhead"),
					tenant.getBranding().getHead(), null, true);
			item.setHeight(com.google.gwt.user.client.Window.getClientHeight() - 100);
			LD.askForValue(I18N.message("customhead"), I18N.message("entercustomhead"), tenant.getBranding().getHead(),
					item, com.google.gwt.user.client.Window.getClientWidth() - 50, value -> {
						tenant.getBranding().setHead(value);
						if (changedHandler != null)
							changedHandler.onChanged(null);
					});
		});
		if (Feature.enabled(Feature.BRANDING_FULL))
			toolStrip.addButton(head);
	}

	private void addCssButton(ChangedHandler changedHandler, ToolStrip toolStrip) {
		ToolStripButton css = new ToolStripButton(I18N.message("css"));
		css.addClickHandler(event -> {
			TextAreaItem item = ItemFactory.newTextAreaItemForAutomation("css", I18N.message("customcss"),
					tenant.getBranding().getCss(), null, false);
			item.setHeight(com.google.gwt.user.client.Window.getClientHeight() - 100);
			LD.askForValue(I18N.message("customcss"), I18N.message("entercustomcss"), tenant.getBranding().getCss(),
					item, com.google.gwt.user.client.Window.getClientWidth() - 50, value -> {
						tenant.getBranding().setCss(value);
						if (changedHandler != null)
							changedHandler.onChanged(null);
					});
		});
		if (Feature.enabled(Feature.BRANDING_FULL)) {
			toolStrip.addButton(css);
			toolStrip.addSeparator();
		}
	}

	public void refresh() {
		boolean readonly = (changedHandler == null);
		vm.clearValues();
		vm.clearErrors(false);
		vm = new ValuesManager();

		if (form != null)
			form.destroy();

		if (Boolean.TRUE.equals(layout.contains(form)))
			layout.removeMember(form);
		if (Boolean.TRUE.equals(layout.contains(imagesPanel)))
			layout.removeMember(imagesPanel);

		skin.setValue(tenant.getBranding().getSkin());

		form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWrapItemTitles(false);
		form.setTitleOrientation(TitleOrientation.TOP);

		boolean fullBranding = Feature.enabled(Feature.BRANDING_FULL);

		boolean readonlyOrNotFullBranding = readonly || !fullBranding;

		TextItem product = ItemFactory.newTextItem("product", tenant.getBranding().getProduct());
		product.setWidth(180);
		product.setDisabled(readonlyOrNotFullBranding);
		product.addChangedHandler(changedHandler);

		TextItem productName = ItemFactory.newTextItem("productname", tenant.getBranding().getProductName());
		productName.setWidth(180);
		productName.setDisabled(readonlyOrNotFullBranding);
		productName.addChangedHandler(changedHandler);

		TextItem vendor = ItemFactory.newTextItem("vendor", tenant.getBranding().getVendor());
		vendor.setWidth(180);
		vendor.setDisabled(readonlyOrNotFullBranding);
		vendor.addChangedHandler(changedHandler);

		TextItem address = ItemFactory.newTextItem("address", tenant.getBranding().getVendorAddress());
		address.setWidth(180);
		address.setDisabled(readonlyOrNotFullBranding);
		address.addChangedHandler(changedHandler);

		TextItem postalCode = ItemFactory.newTextItem("postalcode", tenant.getBranding().getVendorCap());
		postalCode.setWidth(180);
		postalCode.setDisabled(readonlyOrNotFullBranding);
		postalCode.addChangedHandler(changedHandler);

		TextItem city = ItemFactory.newTextItem("city", tenant.getBranding().getVendorCity());
		city.setWidth(180);
		city.setDisabled(readonlyOrNotFullBranding);
		city.addChangedHandler(changedHandler);

		TextItem country = ItemFactory.newTextItem("country", tenant.getBranding().getVendorCountry());
		country.setWidth(180);
		country.setDisabled(readonlyOrNotFullBranding);
		country.addChangedHandler(changedHandler);

		TextItem support = ItemFactory.newEmailItem(SUPPORT, SUPPORT, false);
		support.setValue(tenant.getBranding().getSupport());
		support.setDisabled(readonlyOrNotFullBranding);
		support.setVisible(Feature.enabled(Feature.TECHNICAL_SUPPORT));
		support.addChangedHandler(changedHandler);

		TextItem sales = ItemFactory.newEmailItem(SALES, SALES, false);
		sales.setValue(tenant.getBranding().getSales());
		sales.setEndRow(true);
		sales.setDisabled(readonlyOrNotFullBranding);
		sales.addChangedHandler(changedHandler);

		TextItem webSite = ItemFactory.newTextItem("website", tenant.getBranding().getUrl());
		webSite.setDisabled(readonlyOrNotFullBranding);
		webSite.setColSpan(2);
		webSite.setWidth(360);
		webSite.addChangedHandler(changedHandler);

		TextItem help = ItemFactory.newTextItem("help", tenant.getBranding().getHelp());
		help.setDisabled(readonlyOrNotFullBranding);
		help.setColSpan(2);
		help.setWidth(360);
		help.addChangedHandler(changedHandler);

		TextItem bugs = ItemFactory.newTextItem("bugs", tenant.getBranding().getBugs());
		bugs.setDisabled(readonlyOrNotFullBranding);
		bugs.setVisible(Feature.enabled(Feature.TECHNICAL_SUPPORT));
		bugs.setColSpan(2);
		bugs.setWidth(360);
		bugs.addChangedHandler(changedHandler);

		TextItem forum = ItemFactory.newTextItem("forum", tenant.getBranding().getForum());
		forum.setDisabled(readonlyOrNotFullBranding);
		forum.setColSpan(2);
		forum.setWidth(360);
		forum.addChangedHandler(changedHandler);

		TextItem evaluation = ItemFactory.newTextItem("evaluation", tenant.getBranding().getEvaluation());
		evaluation.setDisabled(readonlyOrNotFullBranding);
		evaluation.setColSpan(2);
		evaluation.setWidth(360);
		evaluation.addChangedHandler(changedHandler);

		form.setItems(product, productName, vendor, address, postalCode, city, country, support, sales, webSite, help,
				forum, bugs, evaluation);

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

		ListGridField nameField = new ListGridField(NAME, I18N.getAttributeLabel(IMAGE));
		nameField.setCanFilter(false);
		nameField.setCanSort(false);
		nameField.setAutoFitWidth(true);
		nameField.setAutoFitWidthApproach(AutoFitWidthApproach.BOTH);

		ListGridField image = prepareImageField();

		grid.setFields(nameField, image);
		
		List<ListGridRecord> records = new ArrayList<>();

		ListGridRecord rec = new ListGridRecord();
		rec.setAttribute(NAME, "logo");
		rec.setAttribute(IMAGE, tenant.getBranding().getLogo());
		records.add(rec);
		rec = new ListGridRecord();
		rec.setAttribute(NAME, LOGO_HEAD);
		rec.setAttribute(IMAGE, tenant.getBranding().getLogoHead());
		records.add(rec);
		rec = new ListGridRecord();
		rec.setAttribute(NAME, LOGO_MENU);
		rec.setAttribute(IMAGE, tenant.getBranding().getLogoMenu());
		records.add(rec);
		rec = new ListGridRecord();
		rec.setAttribute(NAME, BANNER);
		rec.setAttribute(IMAGE, tenant.getBranding().getBanner());
		records.add(rec);

		rec = new ListGridRecord();
		rec.setAttribute(NAME, LOGO_OEM);
		rec.setAttribute(IMAGE, tenant.getBranding().getLogoOem());
		records.add(rec);
		rec = new ListGridRecord();
		rec.setAttribute(NAME, LOGO_HEAD_OEM);
		rec.setAttribute(IMAGE, tenant.getBranding().getLogoHeadOem());
		records.add(rec);
		rec = new ListGridRecord();
		rec.setAttribute(NAME, FAVICON);
		rec.setAttribute(IMAGE, tenant.getBranding().getFavicon());
		records.add(rec);
		grid.setRecords(records.toArray(new ListGridRecord[0]));

		if (changedHandler != null)
			grid.addCellContextClickHandler(event -> {
				showImagesContextMenu();
				event.cancel();
			});

		return grid;
	}

	private ListGridField prepareImageField() {
		ListGridField image = new ListGridField(IMAGE, " ", 300);
		image.setWidth("*");
		image.setCanFilter(false);
		image.setCanSort(false);
		image.setCellFormatter((Object value, ListGridRecord rec, int rowNum, int colNum) -> {
			String html = "";
			String maxWidth = "400px";
			String name = rec.getAttributeAsString(NAME);
			if (name.equals("logo"))
				html = tenant.getBranding().getLogo();
			else if (name.equals(LOGO_HEAD))
				html = tenant.getBranding().getLogoHead();
			else if (name.equals(LOGO_MENU))
				html = tenant.getBranding().getLogoMenu();
			else if (name.equals(LOGO_OEM))
				html = tenant.getBranding().getLogoOem();
			else if (name.equals(LOGO_HEAD_OEM))
				html = tenant.getBranding().getLogoHeadOem();
			else if (name.equals(BANNER))
				html = tenant.getBranding().getBanner();
			else if (name.equals(FAVICON)) {
				html = tenant.getBranding().getFavicon();
				maxWidth = "64px";
			}

			if (html != null && !html.isEmpty()) {			
				html = "<img src='" + html + "' style='margin-top:4px; margin-bottom:4px; max-width: "+maxWidth+";'/>";

				// In case of header logos, show the banner below
				if (name.contains("_head")) {
					html = "<div style=\" background-image: url('" + tenant.getBranding().getBanner()
							+ "'); width:100%; margin:4px; \">" + html + "</div>";
				} else {
					html = "<div style='width:100%; margin-top:4px; margin-bottom:4px;'>" + html + "</div>";
				}
			}
			return html;
		});
		return image;
	}

	private void showImagesContextMenu() {
		final ListGridRecord rec = grid.getSelectedRecord();
		final String name = rec.getAttributeAsString(NAME);

		if ((!Feature.enabled(Feature.BRANDING_LOGO) && !Feature.enabled(Feature.BRANDING_FULL))
				&& !LOGO_OEM.equals(name) && !LOGO_HEAD_OEM.equals(name))
			return;

		Menu contextMenu = new Menu();
		MenuItem reset = new MenuItem();
		reset.setTitle(I18N.message("reset"));
		reset.addClickHandler(event -> {
			GUIBranding model = new GUIBranding();

			String image = "";
			if ("logo".equals(name)) {
				tenant.getBranding().setLogo(model.getLogo());
				image = model.getLogo();
			} else if (LOGO_HEAD.equals(name)) {
				tenant.getBranding().setLogoHead(model.getLogoHead());
				image = model.getLogoHead();
			} else if (LOGO_MENU.equals(name)) {
				tenant.getBranding().setLogoMenu(model.getLogoMenu());
				image = model.getLogoMenu();
			} else if (LOGO_OEM.equals(name)) {
				tenant.getBranding().setLogoOem(model.getLogoOem());
				image = model.getLogoOem();
			} else if (LOGO_HEAD_OEM.equals(name)) {
				tenant.getBranding().setLogoHeadOem(model.getLogoHeadOem());
				image = model.getLogoHeadOem();
			} else if (FAVICON.equals(name)) {
				tenant.getBranding().setFavicon(model.getFavicon());
				image = model.getFavicon();
			} else if (BANNER.equals(name)) {
				tenant.getBranding().setBanner(model.getBanner());
				image = model.getBanner();
			}

			updateImage(name, image);
		});

		MenuItem upload = new MenuItem();
		upload.setTitle(I18N.message("uploadnewimage"));
		upload.addClickHandler(
				event -> new ImageUploader(rec.getAttributeAsString(NAME), TenantBrandingPanel.this).show());

		contextMenu.setItems(upload, reset);
		contextMenu.showContextMenu();
	}

	void updateImage(String imageName, String imageContent) {
		String content = imageContent;
		if (!content.startsWith(GUIBranding.DATA_PREFIX))
			content = GUIBranding.PNG_PREFIX + imageContent;

		if (imageName.equals("logo"))
			tenant.getBranding().setLogo(content);
		else if (imageName.equals(LOGO_HEAD))
			tenant.getBranding().setLogoHead(content);
		else if (imageName.equals(LOGO_MENU))
			tenant.getBranding().setLogoMenu(content);
		else if (imageName.equals(LOGO_OEM))
			tenant.getBranding().setLogoOem(content);
		else if (imageName.equals(LOGO_HEAD_OEM))
			tenant.getBranding().setLogoHeadOem(content);
		else if (imageName.equals(BANNER))
			tenant.getBranding().setBanner(content);
		else if (imageName.equals(FAVICON))
			tenant.getBranding().setFavicon(content);

		Record rec = null;

		// Find the rec the corresponds to the given document
		Record[] records = grid.getRecords();
		for (Record recd : records)
			if (recd.getAttribute(NAME).equals(imageName))
				rec = recd;

		if (rec != null) {
			rec.setAttribute(IMAGE, content);
			grid.invalidateRecordComponents();
			grid.refreshRecordComponent(grid.getRecordIndex(rec));
			grid.redraw();
		}

		if (changedHandler != null)
			changedHandler.onChanged(null);
	}

	public boolean validate() {
		vm.validate();
		tenant.getBranding().setSkin(skin.getValueAsString());
		if (Boolean.FALSE.equals(vm.hasErrors()) && Feature.enabled(Feature.BRANDING_FULL)) {
			tenant.getBranding().setProduct(vm.getValueAsString("product"));
			tenant.getBranding().setProductName(vm.getValueAsString("productname"));
			tenant.getBranding().setVendor(vm.getValueAsString("vendor"));
			tenant.getBranding().setVendorAddress(vm.getValueAsString("address"));
			tenant.getBranding().setVendorCap(vm.getValueAsString("postalcode"));
			tenant.getBranding().setVendorCity(vm.getValueAsString("city"));
			tenant.getBranding().setVendorCountry(vm.getValueAsString("country"));
			tenant.getBranding().setUrl(vm.getValueAsString("website"));
			tenant.getBranding().setBugs(vm.getValueAsString("bugs"));
			tenant.getBranding().setForum(vm.getValueAsString("forum"));
			tenant.getBranding().setEvaluation(vm.getValueAsString("evaluation"));
			tenant.getBranding().setHelp(vm.getValueAsString("help"));
			tenant.getBranding().setSupport(vm.getValueAsString(SUPPORT));
			tenant.getBranding().setSales(vm.getValueAsString(SALES));
		}

		return !vm.hasErrors();
	}

	public void update(GUIBranding branding) {
		tenant.setBranding(branding);
		refresh();
		if (changedHandler != null)
			changedHandler.onChanged(null);
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}