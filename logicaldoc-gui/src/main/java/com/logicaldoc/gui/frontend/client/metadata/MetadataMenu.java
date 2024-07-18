package com.logicaldoc.gui.frontend.client.metadata;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIScheme;
import com.logicaldoc.gui.common.client.beans.GUIWorkflow;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.metadata.barcode.BarcodesPanel;
import com.logicaldoc.gui.frontend.client.metadata.form.FormsPanel;
import com.logicaldoc.gui.frontend.client.metadata.stamp.StampsPanel;
import com.logicaldoc.gui.frontend.client.metadata.tag.TagsPanel;
import com.logicaldoc.gui.frontend.client.metadata.template.TemplatesAndAttributesPanel;
import com.logicaldoc.gui.frontend.client.metadata.zonalocr.ZonalOCRPanel;
import com.logicaldoc.gui.frontend.client.services.SchemeService;
import com.logicaldoc.gui.frontend.client.services.TagService;
import com.logicaldoc.gui.frontend.client.workflow.designer.WorkflowDesigner;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the administration document metadata and workflow menu
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class MetadataMenu extends VLayout {

	public MetadataMenu() {
		setMargin(10);
		setMembersMargin(5);
		setOverflow(Overflow.AUTO);

		addTagsButton();

		addTemplatesButton();

		addCustomidAndAutonamingButton();

		addWorkflowButton();

		addFolderTemplateButton();

		addRetentionPoliciesButton();

		addStampsButton();

		addFormsButton();

		addBarcodeButton();

		Button zonalOcr = new Button(I18N.message("zonalocr"));
		zonalOcr.setWidth100();
		zonalOcr.setHeight(25);
		if (Feature.visible(Feature.ZONAL_OCR) && Menu.enabled(Menu.ZONAL_OCR)) {
			addMember(zonalOcr);
			if (!Feature.enabled(Feature.ZONAL_OCR))
				setFeatureDisabled(zonalOcr);
		}
		zonalOcr.addClickHandler(event -> AdminScreen.get().setContent(new ZonalOCRPanel(null, null)));
	}

	private void setFeatureDisabled(Button button) {
		button.setDisabled(true);
		button.setTooltip(I18N.message("featuredisabled"));
	}

	private void addBarcodeButton() {
		Button barcode = new Button(I18N.message("barcodes"));
		barcode.setWidth100();
		barcode.setHeight(25);
		barcode.addClickHandler(barcodeClick -> AdminScreen.get().setContent(new BarcodesPanel()));
		if (Feature.visible(Feature.BARCODES) && Menu.enabled(Menu.BARCODES)) {
			addMember(barcode);
			if (!Feature.enabled(Feature.BARCODES))
				setFeatureDisabled(barcode);
		}
	}

	private void addFormsButton() {
		Button forms = new Button(I18N.message("forms"));
		forms.setWidth100();
		forms.setHeight(25);
		forms.addClickHandler(click -> AdminScreen.get().setContent(new FormsPanel()));
		if (Feature.visible(Feature.FORM) && Menu.enabled(Menu.FORMS)) {
			addMember(forms);
			if (!Feature.enabled(Feature.FORM))
				setFeatureDisabled(forms);
		}
	}

	private void addStampsButton() {
		Button stamps = new Button(I18N.message("stamps"));
		stamps.setWidth100();
		stamps.setHeight(25);
		stamps.addClickHandler(click -> AdminScreen.get().setContent(new StampsPanel()));
		if (Feature.visible(Feature.STAMP) && Menu.enabled(Menu.STAMPS)) {
			addMember(stamps);
			if (!Feature.enabled(Feature.STAMP))
				setFeatureDisabled(stamps);
		}
	}

	private void addRetentionPoliciesButton() {
		Button retentionPolicies = new Button(I18N.message("retentionpolicies"));
		retentionPolicies.setWidth100();
		retentionPolicies.setHeight(25);
		retentionPolicies
				.addClickHandler(click -> AdminScreen.get().setContent(new RetentionPoliciesPanel()));
		if (Feature.visible(Feature.RETENTION_POLICIES) && Menu.enabled(Menu.RETENTION_POLICIES)) {
			addMember(retentionPolicies);
			if (!Feature.enabled(Feature.RETENTION_POLICIES))
				setFeatureDisabled(retentionPolicies);
		}
	}

	private void addFolderTemplateButton() {
		Button folderTemplates = new Button(I18N.message("foldertemplates"));
		folderTemplates.setWidth100();
		folderTemplates.setHeight(25);
		folderTemplates
				.addClickHandler(click -> AdminScreen.get().setContent(new FolderTemplatesPanel()));
		if (Feature.visible(Feature.FOLDER_TEMPLATE)) {
			addMember(folderTemplates);
			if (!Feature.enabled(Feature.FOLDER_TEMPLATE))
				setFeatureDisabled(folderTemplates);
		}
	}

	private void addWorkflowButton() {
		Button workflow = new Button(I18N.message("workflow"));
		workflow.setWidth100();
		workflow.setHeight(25);
		workflow.addClickHandler(
				workflowClick -> AdminScreen.get().setContent(new WorkflowDesigner(new GUIWorkflow())));
		if (Feature.visible(Feature.WORKFLOW) && Menu.enabled(Menu.WORKFLOW)) {
			addMember(workflow);
			if (!Feature.enabled(Feature.WORKFLOW))
				setFeatureDisabled(workflow);
		}
	}

	private void addCustomidAndAutonamingButton() {
		Button customidAndAutonaming = new Button(I18N.message("customidandnaming"));
		customidAndAutonaming.setWidth100();
		customidAndAutonaming.setHeight(25);
		customidAndAutonaming.addClickHandler(
				customidAndAutonamingClick -> SchemeService.Instance.get().load(new AsyncCallback<>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(List<GUIScheme> schemas) {
						AdminScreen.get().setContent(new CustomIdPanel(schemas));
					}
				}));
		if (Menu.enabled(Menu.CUSTOM_ID) && (Feature.visible(Feature.CUSTOMID) || Feature.visible(Feature.AUTO_NAMING)
				|| Feature.visible(Feature.AUTO_FOLDING))) {
			addMember(customidAndAutonaming);
			if (!Feature.enabled(Feature.CUSTOMID) && !Feature.enabled(Feature.AUTO_NAMING)
					&& !Feature.enabled(Feature.AUTO_FOLDING))
				setFeatureDisabled(customidAndAutonaming);
		}
	}

	private void addTemplatesButton() {
		Button templates = new Button(I18N.message("templates"));
		templates.setWidth100();
		templates.setHeight(25);
		templates.addClickHandler(templatesClick -> AdminScreen.get().setContent(new TemplatesAndAttributesPanel()));
		if (Feature.visible(Feature.TEMPLATE)) {
			addMember(templates);
			if (!Feature.enabled(Feature.TEMPLATE))
				setFeatureDisabled(templates);
		}
	}

	private void addTagsButton() {
		Button tags = new Button(I18N.message("tags"));
		tags.setWidth100();
		tags.setHeight(25);
		if (Feature.visible(Feature.TAGS_ADMIN)) {
			addMember(tags);
			if (!Feature.enabled(Feature.TAGS_ADMIN))
				setFeatureDisabled(tags);
		}
		tags.addClickHandler(tagsClick -> TagService.Instance.get().getSettings(new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(List<GUIParameter> parameters) {
				AdminScreen.get().setContent(new TagsPanel(parameters));
			}
		}));
	}
}