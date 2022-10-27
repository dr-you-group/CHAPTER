# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of CHAPTER
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#Package name
packageFiles <- list.files(path = ".",
                           full.names = TRUE,
                           recursive = T)

packageFiles <- packageFiles[!grepl("Docker", packageFiles)] #Remove files in Docker
for (iFile in packageFiles){
    iText <- readLines(iFile)
    iText2 <- gsub(pattern = "CHAPTER", replace = "CHAPTER", x = iText)
    writeLines(iText2, con = iFile)
}

# Format and check code ---------------------------------------------------
# OhdsiRTools::formatRFolder()
OhdsiRTools::checkUsagePackage("CHAPTER")
OhdsiRTools::updateCopyrightYearFolder()

# Create manual -----------------------------------------------------------
unlink("extras/UsingSkeletonPackage.pdf")
shell("R CMD Rd2pdf ./ --output=extras/UsingSkeletonPackage.pdf")

# Store environment in which the study was executed -----------------------
OhdsiRTools::createRenvLockFile(rootPackage = "CHAPTER",
                                mode = "description",
                                ohdsiGitHubPackages = unique(c(OhdsiRTools::getOhdsiGitHubPackages(),
                                                               "CohortGenerator")),
                                includeRootPackage = FALSE)



