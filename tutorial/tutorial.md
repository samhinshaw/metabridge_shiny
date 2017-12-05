<h2 id='metabolite-preprocessing'>Metabolite Preprocessing</h2>

Preprocess your metabolites in your method of choice. We recommend
[MetaboAnalyst](http://www.metaboanalyst.ca) for metabolite preprocessing.

![MetaboAnalyst](serve/00_metaboanalyst.png)

---

<h2 id='metabridge-mapping'>MetaBridge Mapping</h2>

### Upload Metabolites

![Upload Metabolites](serve/01_upload_metabolites.png)

### Map Metabolites

![Map Via MetaCyc](serve/02_map_metacyc.png)

### Result

<table class="table table-dark table-hover table-bordered">
  <thead class="thead-dark">
    <tr>
      <th>MetaCyc Gene</th>
      <th>Gene Name</th>
      <th>Ensembl</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>HS08579</td>
      <td>BDH1</td>
      <td>ENSG00000161267</td>
    </tr>
    <tr>
      <td>HS08987</td>
      <td>BDH2</td>
      <td>ENSG00000164039</td>
    </tr>
    <tr>
      <td>HS04116</td>
      <td>HMGCL</td>
      <td>ENSG00000117305</td>
    </tr>
    <tr>
      <td>G-12374</td>
      <td>liuE</td>
      <td>NA</td>
    </tr>
    <tr>
      <td>G-9191</td>
      <td>mvaB</td>
      <td>NA</td>
    </tr>
    <tr>
      <td>G-10295</td>
      <td>kce</td>
      <td>NA</td>
    </tr>
    <tr>
      <td>HS02536</td>
      <td>FAH</td>
      <td>ENSG00000103876</td>
    </tr>
  </tbody>
</table>

---

<h2 id='networkanalyst'>NetworkAnalyst</h2>

![Choose Protein-Protein Interactions and IMEX Interactome](serve/04_interaction_db.png)

![Upload Genes to NetworkAnalyst](serve/05_na_gene_symbol.png)

### Create Networks

![Create Minimum-Connected Network](serve/06_minimum_network_gen.png)

![Metabolomic Minimum-Connected Network](serve/07_minimum_network_topology.png)

![Transcriptomic Minimum-Connected Network](serve/08_transcript_network.png)

![Integrated Minimum-Connected Network](serve/09_combined_network.png)
