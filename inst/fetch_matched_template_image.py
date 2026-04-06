import nibabel as nib
import re
from templateflow import api
from nilearn.image import resample_to_img

def parse_entity_from_filename(filename, entity):
    match = re.search(rf"{entity}-([a-zA-Z0-9]+)", filename)
    return match.group(1) if match else None

def normalize_templateflow_value(value):
    if value is None:
        return None
    if isinstance(value, str):
        value = value.strip()
        if value == "" or value.lower() == "none":
            return None
        if re.fullmatch(r"\d+", value):
            return int(value)
    return value

# Helper function to get the voxel resolution from a NIfTI file
def get_voxel_resolution(img):
    zooms = img.header.get_zooms()[:3]
    res = tuple(round(z, 3) for z in zooms)
    return res

# Helper function to parse the image space from the filename
def parse_space_from_filename(filename):
    return parse_entity_from_filename(filename, "space") or "T1w"

# Fetch the template image from TemplateFlow
def fetch_template_image(template, resolution, suffix, desc=None, extension=".nii.gz", cohort=None):
    cohort = normalize_templateflow_value(cohort)
    print(
        f"Fetching from TemplateFlow: template={template}, cohort={cohort}, "
        f"desc={desc}, resolution={resolution}, suffix={suffix}"
    )
    query = dict(
        template=template,
        desc=desc,
        resolution=resolution,
        suffix=suffix,
        extension=extension
    )
    if cohort is not None:
        query["cohort"] = cohort
    return api.get(**query)

def resample_template_to_bold(in_file, output, template_resolution=1, template_space=None,
    template_cohort=None, suffix="mask", desc="brain", extension=".nii.gz", interpolation="nearest"):
    
    # detect the space from the filename if not provided
    if template_space is None:
        template_space = parse_space_from_filename(in_file)
    if template_cohort is None:
        template_cohort = parse_entity_from_filename(in_file, "cohort")
    template_cohort = normalize_templateflow_value(template_cohort)
    
    bold_img = nib.load(in_file)
    resolution = get_voxel_resolution(bold_img)
    print(f"Detected space: {template_space}, cohort: {template_cohort}, resolution: {resolution}")

    # grab the template image from templateflow
    image_path = fetch_template_image(
        template=template_space,
        resolution=template_resolution,
        cohort=template_cohort,
        suffix=suffix,
        desc=desc,
        extension=extension
    )

    if isinstance(image_path, (list, tuple)):
        if len(image_path) == 1:
            image_path = image_path[0]
        else:
            raise ValueError(
                f"TemplateFlow returned multiple files for template={template_space}, "
                f"cohort={template_cohort}, desc={desc}, resolution={template_resolution}"
            )

    template_img = nib.load(image_path)
    # https://nilearn.github.io/stable/auto_examples/06_manipulating_images/plot_resample_to_template.html
    resampled_mask = resample_to_img(
        source_img = template_img, target_img = bold_img, interpolation = interpolation, 
        force_resample = True, copy_header = True)
    nib.save(resampled_mask, output)
    return output
