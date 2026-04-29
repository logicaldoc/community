package com.logicaldoc.core.filler;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.filter.AnnotationTypeFilter;

import com.logicaldoc.core.PersistenceException;
import com.logicaldoc.core.PersistentObject;
import com.logicaldoc.core.automation.Automation;
import com.logicaldoc.core.automation.AutomationException;
import com.logicaldoc.core.document.Document;
import com.logicaldoc.core.document.DocumentHistory;
import com.logicaldoc.core.document.DocumentManager;
import com.logicaldoc.core.document.IndexingStatus;
import com.logicaldoc.core.history.History;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.core.parser.ParsingException;
import com.logicaldoc.core.runtime.Aspect;
import com.logicaldoc.core.runtime.FeatureDisabledException;
import com.logicaldoc.core.runtime.RunLevel;
import com.logicaldoc.core.searchengine.Hit;
import com.logicaldoc.core.searchengine.SearchEngine;
import com.logicaldoc.core.searchengine.SearchException;

import jakarta.persistence.Column;
import jakarta.persistence.DiscriminatorColumn;
import jakarta.persistence.DiscriminatorType;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import jakarta.persistence.Table;

/**
 * A Filler implements its own logic to auto-fill the metadata of an
 * {@link ExtensibleObject}
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2.3
 */
@Entity
@Table(name = "ld_filler")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@DiscriminatorColumn(name = "ld_type", discriminatorType = DiscriminatorType.STRING)
public abstract class Filler extends PersistentObject {

    private static final long serialVersionUID = 1L;

    private static final Logger log = LoggerFactory.getLogger(Filler.class);

    @Column(name = "ld_name", length = 255, nullable = false)
    protected String name;

    @Column(name = "ld_label", length = 255, nullable = true)
    protected String label;

    @Column(name = "ld_description", nullable = true)
    protected String description;

    /**
     * Specifies if the document must be re-filled at checkin
     */
    @Column(name = "ld_checkin", nullable = false)
    protected boolean checkin = false;

    /**
     * Specifies if already filled properties must be overwritten
     */
    @Column(name = "ld_overwrite", nullable = false)
    protected boolean overwrite = false;

    /**
     * Optional automation procedure invoked after the filling
     */
    @Column(name = "ld_automation", nullable = true)
    protected String automation;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public boolean isCheckin() {
        return checkin;
    }

    public void setCheckin(boolean checkin) {
        this.checkin = checkin;
    }

    public boolean isOverwrite() {
        return overwrite;
    }

    public void setOverwrite(boolean overwrite) {
        this.overwrite = overwrite;
    }

    public String getAutomation() {
        return automation;
    }

    public void setAutomation(String automation) {
        this.automation = automation;
    }

    /**
     * Factory method for instantiating a new filler
     * 
     * @param type Type of filler, matches the discriminator value
     * 
     * @return The new filler
     * 
     * @throws IllegalArgumentException No implementation found for the given
     *         type
     */
    public static Filler newFiller(String type) {
        ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
        scanner.addIncludeFilter(new AnnotationTypeFilter(DiscriminatorValue.class));
        for (BeanDefinition bd : scanner.findCandidateComponents("com.logicaldoc")) {
            try {
                Class<?> beanClass = Class.forName(bd.getBeanClassName());
                if (Filler.class.isAssignableFrom(beanClass)
                        && type.equals(beanClass.getAnnotation(DiscriminatorValue.class).value()))
                    return (Filler) beanClass.getDeclaredConstructor().newInstance();
            } catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException
                    | InvocationTargetException | NoSuchMethodException | SecurityException e) {
                log.warn(e.getMessage(), e);
            }
        }

        throw new IllegalArgumentException("Cannot find any filler of type %s".formatted(type));
    }

    /**
     * Fills an object instance
     * 
     * @param fillable the instance to fill
     * @param content the content of the object, if not specified it will be
     *        taken from the transaction's file.
     * @param transaction the current transaction
     * @param dictionary Dictionary of the execution pipeline
     * 
     * @throws PersistenceException Error in the data layer
     * @throws IOException I/O error
     * @throws FeatureDisabledException An involved feature is disabled
     * @throws SearchException Error in case of search
     */
    protected abstract void fill(Document fillable, String content, History transaction, Map<String, Object> dictionary)
            throws PersistenceException, IOException, FeatureDisabledException, SearchException;

    /**
     * Fills a document using the body text as input
     * 
     * @param document the document to fill
     * @param transaction the current transaction
     * @param dictionary Dictionary of the execution pipeline
     * 
     * @throws PersistenceException Error in the data layer
     * @throws IOException I/O error
     * @throws ParsingException The document cannot be parsed and no texts were
     *         extracted
     * @throws FeatureDisabledException An involved feature is disabled
     * @throws SearchException Error in case of search
     * @throws AutomationException Error in the automation execution(if any
     *         automation script has been provided)
     */
    public void fill(Document document, DocumentHistory transaction, Map<String, Object> dictionary)
            throws PersistenceException, IOException, FeatureDisabledException, ParsingException, SearchException,
            AutomationException {

        if (!RunLevel.current().aspectEnabled(Aspect.AUTOFILL) || document.getFormId() != null)
            return;

        if (document.getIndexingStatus().equals(IndexingStatus.TO_INDEX)) {
            DocumentManager.get().index(document.getId(), null, new DocumentHistory(transaction));
            document.setIndexingStatus(IndexingStatus.INDEXED);
        }

        Hit hit = SearchEngine.get().getHit(document.getId());
        String extractedContent = hit != null ? hit.getContent() : "";
        if (StringUtils.isBlank(extractedContent))
            throw new ParsingException("Cannot extract any content from document %s".formatted(document));

        if (log.isDebugEnabled())
            log.debug("Filling documnent {} using text {}", document, StringUtils.abbreviate(extractedContent, 150));

        fill(document, extractedContent, transaction, dictionary);

        if (StringUtils.isNotEmpty(automation)) {
            Automation script = new Automation("Filler-%s".formatted(name), null, getTenantId());
            script.evaluate(automation, Map.of("filler", this, "document", document, "transaction", transaction,
                    "fillerDictionary", dictionary));
        }
    }

    protected boolean mustOverwrite(History transaction) {
        if (transaction.getOverwrite() != null)
            return transaction.getOverwrite().booleanValue();
        else
            return isOverwrite();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((label == null) ? 0 : label.hashCode());
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        Filler other = (Filler) obj;
        if (label == null) {
            if (other.label != null)
                return false;
        } else if (!label.equals(other.label))
            return false;
        if (name == null)
            return other.name == null;
        else
            return name.equals(other.name);
    }

    @Override
    public String toString() {
        return "%s(%d)".formatted(name, id);
    }
}