resource "a" "b" {
  reference = local.value
  reference = data.other_resource.id.attr
}
