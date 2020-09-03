import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") apply false
    id("java")
    id("maven-publish")
}


group = "org.jetbrains.dokka"
version = "0.1.1-SNAPSHOT"

val language_version: String by project

tasks.withType(KotlinCompile::class).all {
    kotlinOptions {
        freeCompilerArgs += "-Xjsr305=strict -Xskip-metadata-version-check -Xopt-in=kotlin.RequiresOptIn."
        languageVersion = language_version
        apiVersion = language_version
        jvmTarget = "1.8"
    }
}

repositories {
    jcenter()
    mavenCentral()
    maven(url = "https://dl.bintray.com/kotlin/kotlin-eap")
    maven(url = "https://dl.bintray.com/kotlin/kotlin-dev")
}

dependencies {
    implementation("org.jetbrains.dokka:dokka-core:1.4.0-rc")
    implementation("org.jetbrains.dokka:dokka-base:1.4.0-rc")
    implementation("com.vladsch.flexmark:flexmark-all:0.42.12")
    implementation("nl.big-o:liqp:0.6.7")
    implementation("org.jetbrains.kotlinx:kotlinx-html-jvm:0.7.1")
    implementation("junit:junit:4.13")
}

apply {
    plugin("org.jetbrains.kotlin.jvm")
    plugin("java")
}

// Gradle metadata
java {
    @Suppress("UnstableApiUsage")
    withSourcesJar()
    targetCompatibility = JavaVersion.VERSION_1_8
}

publishing {
    publications {
        create<MavenPublication>("maven") {
            groupId = "scala3doc"
            artifactId = "dokka-java-api"
            version = "0.1.1-SNAPSHOT"

            from(components["java"])
        }
    }
}

// Workaround for https://github.com/bintray/gradle-bintray-plugin/issues/267
//  Manually disable bintray tasks added to the root project
tasks.whenTaskAdded {
    if ("bintray" in name) {
        enabled = false
    }
}
