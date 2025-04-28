package ahc.dms.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.env.Environment;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.orm.jpa.JpaTransactionManager;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import javax.sql.DataSource;
import java.util.HashMap;
import java.util.Objects;

// Configuration for PostgreSQL edms
@Configuration
@PropertySource({ "classpath:persistence-ahc.properties" })
@EnableTransactionManagement
@EnableJpaRepositories(
        basePackages = "ahc.dms.dao.pgdms2.repositories",
        entityManagerFactoryRef = "pgDms2EntityManager",
        transactionManagerRef = "pgDms2TransactionManager"
)
public class PgDms2Config {

    @Autowired
    private Environment env;

    @Bean(name = "pgDms2DataSource")
    public DataSource pgDms2DataSource() {
        DriverManagerDataSource dataSource = new DriverManagerDataSource();
        dataSource.setDriverClassName(Objects.requireNonNull(env.getProperty("spring.datasource.pg_dms2.driverClassName")));
        dataSource.setUrl(env.getProperty("spring.datasource.pg_dms2.url"));
        dataSource.setUsername(env.getProperty("spring.datasource.pg_dms2.username"));
        dataSource.setPassword(env.getProperty("spring.datasource.pg_dms2.password"));

        return dataSource;
    }

    @Bean(name = "pgDms2EntityManager")
    public LocalContainerEntityManagerFactoryBean pgDms2EntityManager() {

        HashMap<String, Object> jpaProperties = new HashMap<>();
        jpaProperties.put("hibernate.hbm2ddl.auto", env.getProperty("spring.jpa.pg_dms2.properties.hibernate.hbm2ddl.auto"));
        jpaProperties.put("hibernate.dialect", env.getProperty("spring.jpa.pg_dms2.properties.hibernate.dialect"));
        jpaProperties.put("hibernate.show_sql", "spring.jpa.pg_dms2.properties.show-sql");
        jpaProperties.put("hibernate.format_sql", "spring.jpa.pg_dms2.properties.format-sql");

        LocalContainerEntityManagerFactoryBean emf = new LocalContainerEntityManagerFactoryBean();
        emf.setDataSource(pgDms2DataSource());
        emf.setPackagesToScan("ahc.dms.dao.pgdms2.entities");
        emf.setJpaPropertyMap(jpaProperties);
        emf.setPersistenceUnitName("pgDms2");
        emf.setJpaVendorAdapter(new HibernateJpaVendorAdapter());

        return emf;
    }

    @Bean(name = "pgDms2TransactionManager")
    public PlatformTransactionManager pgDms2TransactionManager() {

        JpaTransactionManager transactionManager = new JpaTransactionManager();
        transactionManager.setEntityManagerFactory(pgDms2EntityManager().getObject());
        return transactionManager;
    }
}